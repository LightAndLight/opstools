# `opstools`

Tools for managing software deployments.

## Contents

* [`sshgen`](#sshgen)
* [`tmply`](#tmply)

## `sshgen`

Simplified interface to `ssh-keygen` for stdin/stdout-based SSH key generation. 

Usage: `nix run github:LightAndLight/opstools#sshgen -- --help`

## `tmply`

Pass temporary data to a process and clean up after.

Usage: `nix run github:LightAndLight/opstools#tmply -- --help`

Example: `tmply -i <(getKnownHosts) -i <(getPrivateKey) -- ssh -o UserKnownHosts={0} -i {1} user@example.com`

## `retry`

Retry a failing command

Usage: `nix run github:LightAndLight/opstools#retry -- --help`

Example: `retry --num-retries 5 --delay 5 -- ssh-keyscan example.com`
