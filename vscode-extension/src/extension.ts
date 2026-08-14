import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;

const BINARY_NAME = process.platform === 'win32' ? 'fp-lsp.exe' : 'fp-lsp';

export function activate(context: vscode.ExtensionContext): void {
  outputChannel = vscode.window.createOutputChannel('FerroPhase');
  context.subscriptions.push(outputChannel);

  const serverPath = findServerBinary();
  if (!serverPath) {
    outputChannel.appendLine(
      `No '${BINARY_NAME}' binary found (checked 'ferrophase.serverPath' setting, PATH, and target/debug|release). ` +
        'Syntax highlighting will still work; language server features are disabled.'
    );
    return;
  }

  outputChannel.appendLine(`Found fp-lsp binary at: ${serverPath}`);

  try {
    const serverOptions: ServerOptions = {
      run: { command: serverPath, transport: TransportKind.stdio },
      debug: { command: serverPath, transport: TransportKind.stdio },
    };

    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: 'file', language: 'ferrophase' }],
      outputChannel,
    };

    client = new LanguageClient(
      'ferrophaseLanguageServer',
      'FerroPhase Language Server',
      serverOptions,
      clientOptions
    );

    client.start().then(
      () => outputChannel.appendLine('FerroPhase language server started.'),
      (err: unknown) =>
        outputChannel.appendLine(
          `Failed to start FerroPhase language server: ${String(err)}`
        )
    );
  } catch (err) {
    outputChannel.appendLine(
      `Error initializing FerroPhase language client: ${String(err)}`
    );
    client = undefined;
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

/**
 * Locate the fp-lsp binary. Checks, in order:
 * 1. The `ferrophase.serverPath` setting.
 * 2. Directories on PATH.
 * 3. `target/debug` / `target/release` relative to each open workspace folder
 *    (useful since this extension lives inside the FerroPhase monorepo during development).
 */
function findServerBinary(): string | undefined {
  const config = vscode.workspace.getConfiguration('ferrophase');
  const configuredPath = config.get<string>('serverPath');
  if (configuredPath && configuredPath.trim().length > 0) {
    if (fs.existsSync(configuredPath)) {
      return configuredPath;
    }
    outputChannel.appendLine(
      `Configured 'ferrophase.serverPath' does not exist: ${configuredPath}`
    );
  }

  const fromPath = findOnPath(BINARY_NAME);
  if (fromPath) {
    return fromPath;
  }

  for (const folder of vscode.workspace.workspaceFolders ?? []) {
    for (const profile of ['debug', 'release']) {
      const candidate = path.join(
        folder.uri.fsPath,
        'target',
        profile,
        BINARY_NAME
      );
      if (fs.existsSync(candidate)) {
        return candidate;
      }
    }
  }

  return undefined;
}

function findOnPath(binaryName: string): string | undefined {
  const pathEnv = process.env.PATH ?? process.env.Path;
  if (!pathEnv) {
    return undefined;
  }
  const dirs = pathEnv.split(path.delimiter);
  for (const dir of dirs) {
    const candidate = path.join(dir, binaryName);
    try {
      if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) {
        return candidate;
      }
    } catch {
      // Ignore unreadable PATH entries.
    }
  }
  return undefined;
}
