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
will produce a `plan.json` file that is required for the `cabal-cache` tool to know which built
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

## The archive

### Archive tarball format

Built packages are stored in tarballs which contain the following files:

```bash
x ${compiler_id}/${package_id}/_CC_METADATA/store-path
x ${compiler_id}/lib/libHS${package_id}-*.dylib
x ${compiler_id}/${package_id}
x ${compiler_id}/package.db/${package_id}.conf
```

Aside from the files in the `_CC_METADATA` directory, everything else is copied verbatim from cabal
store from the corresponding location.  This includes the `conf` file which may contain absolute paths
that would cause the built package to be non-relocatable.

As a work-around, the tarball also inclues the `_CC_METADATA/store-path`
file which stores the cabal store path from which the cached package was derived.

Upon unpacking, `cabal-cache` will rewrite the `conf` file to contain the new store path using the
information store in the `_CC_METADATA/store-path` file.  `_CC_METADATA` directory and its contents
will be additionally unpacked making it easy to recognise packages that have been restored using
`cabal-cache`.

### Archive directory structure

The archive contains files in the following locations:

```bash
/Users/jky/moo-archive/${archive_version}/${compiler_id}/${package_id}.tar.gz
/Users/jky/moo-archive/${archive_version}/${store_hash}/${compiler_id}/${package_id}.tar.gz
```

Both tarballs are identical.  If they both exist then the first may be a symlink to the second
when store on the filesystem.

The direct subdirectories of the archive is the `${archive_verson}`, for example `v2`.  This is the
version of the archive format.  This corresponds to the major version of the `cabal-cache` package.

The next directory may be the `${store_hash}` or the `${compiler_id}`.  If it is the `${store_hash}`
then the `${compiler_id}` will be a subdirectory of that.

The `${store_hash}` is the hash of the store path from which the cached package originally came.

`cabal-cache` will preferentially restore using this version if it is available and the `${store_hash}`
matches the cabal store path that is being restore to.

If the package matching the `${store_hash}` cannot be found, `cabal-cache` will fallback to the version
without the `${store_hash}`.

A version without a `${store-hash}` may not exist.  See [Caveats](#caveats) for more information.

## Caveats

### Packages that use absolute paths to the cabal store

Packages sometimes do things that cause their built artefacts to contain absolute paths to the cabal
store.  This unfortunately makes such built packages non-relocatable.

It is recommended that you use a fixed cabal store path rather than the default `$HOME/.cabal/store`
to avoid any potential issues.

See https://github.com/haskell/cabal/issues/4097 for more information.

Following are examples of how this might happen:

#### Paths_$pkgname

`Paths_$pkgname` modules have embedded within them the absolute path to the package in the cabal store
which means that packages that use some features of this module are not relocatable depending on what
they do.

Packages may query this module to get access to the package's cabal store `share` directory which
contains data files that the package can read at runtime.  Using `cabal-cache` for such packages
could mean that the package will be unable to find such data files.

To protect against this, `cabal-cache` will by default not sync packages down from the archive
if the package's cabal store `share` directory contain unusual files or directories _unless_ the
`${store_hash}` matches.  Currently it only considers the `doc` subdirectory to be usual.  More
exceptions may be added later.

