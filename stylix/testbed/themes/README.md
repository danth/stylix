# Testbed themes

This directory contains modules that configure various themes used in the
testbeds.

The total number of testbeds is calculated as `target × theme`, where `target`
is the number of `modules/«target»/testbeds/«target».nix` files and `theme` is
the number of `stylix/testbed/themes/«theme».nix` files.

> [!CAUTION]
> Do not add themes unnecessarily. Doing so will dramatically increase the size
> of the test suite!
