# Packaging
Everything related to packaging should go in this directory. 

# Static Linux binary
This contains singularity definition files. The following explains how to build in sandbox mode. From the project root, to build an image:

    singularity build -f --sandbox /tmp/alpine-entangled packaging/alpine-build.def

To make the static binary distribution:

    singularity run -f --no-home --bind .:/mnt --writable /tmp/alpine-entangled

The resulting tarball has the following structure:

    .
    ├── bin
    │   └── entangled
    ├── lib
    │   └── entangled
    │       ├── data
    │       │   ├── config-schema.dhall
    │       │   ├── example-config.dhall
    │       │   ├── minimal-config.dhall
    │       │   └── schema.sql
    │       └── entangled
    └── share
        └── doc
            └── entangled
                ├── CITATION.cff
                ├── LICENSE
                └── README.md

# Fedora

- [Packaging instructions](https://docs.fedoraproject.org/en-US/quick-docs/creating-rpm-packages/)
