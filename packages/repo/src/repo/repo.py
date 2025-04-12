#!/usr/bin/env python3
import argparse
import logging
import os
import shlex
import subprocess
import sys
from typing import List, Any

logger = logging.getLogger('repo')


def get_parser():
    parser = argparse.ArgumentParser(description='helper for submodules')
    subparsers = parser.add_subparsers(dest='subcommand', required=True)

    submodule = subparsers.add_parser(name='submodule', help='manage git submodules')
    submodule_subparsers = submodule.add_subparsers(dest='action', required=True)
    submodule_init = submodule_subparsers.add_parser('init', help='initialize submodules')
    submodule_deinit = submodule_subparsers.add_parser('deinit', help='deinitialize submodules')
    submodule_deinit.add_argument('path', help='path to deinitialize')

    submodule_update = submodule_subparsers.add_parser('update', help='update submodules')
    submodule_update.add_argument('--remote', action='store_true',
                                  help='fetch the latest changes from the remote repository')
    submodule_list = submodule_subparsers.add_parser('list', help='list submodules')
    submodule_switch = submodule_subparsers.add_parser('switch', help='switch submodules')
    submodule_switch.add_argument('rev', help='revision to switch to')

    subtree = subparsers.add_parser(name='subtree', help='manage git subtrees')
    subtree_subparsers = subtree.add_subparsers(dest='action', required=True)
    subtree_replace = subtree_subparsers.add_parser('replace', help='replace submodule with subtree')
    subtree_replace.add_argument('path', help='path to replace')

    workspace = subparsers.add_parser(name='workspace', help='manage cargo workspace')
    workspace_subparsers = workspace.add_subparsers(dest='action', required=True)
    workspace_init = workspace_subparsers.add_parser('init', help='add a new crate to the workspace')

    return parser


def execute_captured(commands: str | List[str]):
    """
    Executes a shell command and returns the output or an error message.

    Parameters:
    - command: List of command arguments.

    Returns:
    - output: Standard output from the command.
    - error: Standard error message (if any).
    """
    if isinstance(commands, list):
        commands = shlex.join(commands)

    logger.debug(f'Executing command: %s', commands)
    try:
        result = subprocess.run(commands, capture_output=True, text=True, check=True, shell=True)
        return result.stdout, None  # No error
    except subprocess.CalledProcessError as e:
        return None, e.stderr  # Return error message


def execute_piped(commands: str | List[str]) -> bool:
    """
    Executes a shell command and returns the output or an error message.

    Parameters:
    - command: List of command arguments.

    Returns:
    - output: Standard output from the command.
    - error: Standard error message (if any).
    """
    if isinstance(commands, list):
        commands = shlex.join(commands)
    logger.debug(f'Executing command: %s', commands)
    try:
        subprocess.run(commands, capture_output=False, text=True, check=True, shell=True)
        return True
    except subprocess.CalledProcessError as e:
        logger.error(f'Error executing command: %s', e.returncode)
        return False


def get_submodules():
    output, error = execute_captured("git config --file .gitmodules --get-regexp path | awk '{ print $2 }'")
    if error:
        logger.error(f'Error getting submodules: {error}')
        return
    submodules = output.split()
    logger.debug(f'Submodules: %s', submodules)
    return submodules


def submodule_update(args: Any):
    logger.debug('Updating submodules')
    remote = ''
    try:
        if args.remote:
            remote = '--remote'
    except AttributeError:
        pass

    success_modules = []
    error_modules = []

    def submodule_update_inner(path):
        os.chdir(path)
        submodules = get_submodules()
        for submodule in submodules:
            os.chdir(path)
            logger.debug('Updating submodule: %s', submodule)
            command = ['git', 'submodule', 'update', '--init', submodule]
            if remote:
                command.append(remote)
            ok = execute_piped(command)
            if ok:
                logger.info('Submodule updated: %s', submodule)
                success_modules.append(submodule)
                submodule_update_inner(os.path.join(path, submodule))
            else:
                logger.error('Error updating submodule: %s', submodule)
                error_modules.append(submodule)

    submodule_update_inner(os.getcwd())

    logger.info('Successfully updated submodules: %s', success_modules)
    logger.error('Error updating submodules: %s', error_modules)
    logger.info('Submodule update complete(%s/%s)', len(success_modules), len(success_modules) + len(error_modules))


def submodule_deinit(args):
    path = args.path
    execute_piped(f'git submodule deinit -f {path}')
    execute_piped(f'git rm --cached -rf {path}')
    execute_piped(f'rm -rf {path}')
    execute_piped(f'rm -rf .git/modules/{path}')
    input(f'Please edit .gitmodules and remove {path}. Press enter to continue')


def switch_submodules(args):
    execute_piped(['git', 'submodule', 'deinit', '--all', '-f'])
    execute_piped(['git', 'switch', '-d', '-f', args.rev])
    execute_piped(['git', 'clean', '-f', '-d', '.'])

    submodule_update({})


def subtree_replace(args):
    path = args.path
    submodule_deinit(args)
    execute_piped(f'git add .')
    execute_piped(f"git commit -m 'temp: remove submodule {path}'")

    execute_piped(f'git subtree add --prefix {path} https://github.com/SakuraCapital/{path} main')


# well it does not work. cargo metadata only works if all paths are resolved
def list_workspace_members():
    output, error = execute_captured("cargo metadata --format-version 1 --no-deps | jq -r '.packages[].name'")
    if error:
        logger.error(f'Error getting workspace members: {error}')
        return
    members = output.split()
    logger.debug(f'Workspace members: %s', members)
    return members


def workspace_init(args):
    # for each item in the workspace, check if the corresponding Cargo.toml exists
    # if it does not exist, comment it out in the workspace
    members = list_workspace_members()
    comment_member = []
    for member in members:
        logger.debug(f'Checking if Cargo.toml exists for: %s', member)
        if not os.path.exists(member + '/Cargo.toml'):
            comment_member.append(member)
    content = open('Cargo.toml').read()
    for member in comment_member:
        logger.debug(f'Commenting out member: %s', member)
        content = content.replace(f'"{member},"', f'# "{member},"')
    with open('Cargo.toml', 'w') as f:
        f.write(content)


def main():
    parser = get_parser()
    args = parser.parse_args()

    if args.subcommand == 'submodule':
        if args.action == 'init':
            submodule_update(args)
        elif args.action == 'deinit':
            submodule_deinit(args)
        elif args.action == 'update':
            submodule_update(args)
        elif args.action == 'list':
            get_submodules()
        elif args.action == 'switch':
            switch_submodules(args)
    if args.subcommand == 'subtree':
        if args.action == 'replace':
            subtree_replace(args)
    elif args.subcommand == 'workspace':
        if args.action == 'init':
            workspace_init(args)


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG, stream=sys.stdout)
    main()
