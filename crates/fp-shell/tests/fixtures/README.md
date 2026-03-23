`pyinfra_port/` contains adapted `fp-shell` parity cases that compile FP source and assert
generated shell fragments.

`pyinfra_upstream/` is a raw local mirror of upstream pyinfra test data copied from
`../pyinfra/tests/operations` and `../pyinfra/tests/test_connectors`. Keep this tree close to
upstream so new parity cases can be derived incrementally without re-importing fixtures.
