# BOSS
BOSS (BEAM-based Operating System with Security) is a **proof-of-concept**
operating system heavily inspired by the BEAM virtual machine and Erlang/OTP.
It does not have a clear kernel-userspace divide.

## Goals
  - learn Rust;
  - implement an Erlang VM;
  - implement the dumbest fucking idea that came to my head at 3 in the morning.

I am not claiming that my code is safe, fast, secure nor correct. This project
is purely for my education and enjoyment.

## Structure
The basis of this project is the **emulator**, a multiprocessing-aware Erlang VM
that runs on bare hardware. It only implements the bare minimum it has to
implement in order to reach this objective; for example, it _does_ parse some
ACPI tables (since that is required for multiprocessing to work), but it does
not run AML bytecode - that task is left up to the BEAM bytecode it runs. The
emulator only supports the x86_64 ISA, UEFI and ACPI 2.0+, i.e. it will run on
machines from approximately 2010 onwards. It _could_ be argued that the emulator
is a microkernel, since it implements the things a uK would (scheduling and
IPC), but i prefer not to name it that.

On startup, the emulator loads the **base image** (`BOSBAIMA.TAR`) that contains
just enough BEAM modules to bootstrap a functional OS.

As was previously said, there is no clear kernel-userspace divide; instead, BOSS
goes for an Erlang/OTP-like supervision tree model.