# cabal-cache
[![master](https://circleci.com/gh/haskell-works/cabal-cache/tree/master.svg?style=svg)](https://circleci.com/gh/haskell-works/cabal-cache/tree/master)

Tool for caching built cabal new-build packages.

The tool is useful in development when you want to share your build haskell package dependencies of
of a particular project with another developer and also in CI where caching is useful for reducing
build times.

`cabal-cache` supports syncing to an archive directory or to an S3 bucket.

## Installation

Several installation methods are available.

### From source

```bash
cabal new-install cabal-cache
```

### Ubuntu binaries

Dowload Ubuntu binaries from https://github.com/haskell-works/cabal-cache/releases

### Using Homebrew on Mac OS X

```bash
brew tap haskell-works/homebrew-haskell-works git@github.com:haskell-works/homebrew-haskell-works.git
brew update
brew install cabal-cache
```

## Example usage

Syncing built packages with S3 requires you have an S3 bucket with AWS
credentials stored in the `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` environent variables.
You should also know the AWS region the bucket was created in.

### Sync to archive

Change into your project directory.

Build the project with `cabal v2-build`.  This will ensure your dependencies are built and
will product a `plan.json` file that is required for the `cabal-cache` tool to know which built
packages to sync up.

Run the following command to sync to S3.

```bash
cabal-cache sync-to-archive --threads 16 --archive-uri s3://my-cabal-cache-bucket/archive --region Sydney
```

Run the following command to sync to archive directory.

```bash
cabal-cache sync-to-archive --threads 16 --archive-uri archive --region Sydney
```

### Sync from S3

Change into your project directory.

Build the project with `cabal v2-configure`.  This will product a `plan.json` file that is required
for the `cabal-cache` tool to know which built packages to sync down.

Run the following command to sync from S3.

```bash
cabal-cache sync-from-archive --threads 16 --archive-uri s3://my-cabal-cache-bucket/archive --region Sydney
```

Run the following command to sync from archive directory.

```bash
cabal-cache sync-from-archive --threads 16 --archive-uri archive --region Sydney
```

