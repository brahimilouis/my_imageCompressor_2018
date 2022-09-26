cmake_minimum_required(VERSION 3.13)
project(projet)

set(CMAKE_CXX_STANDARD 14)

add_executable(projet
        app/Main
        app/Main.hi
        app/Main.hs
        app/Main.o
        src/Lib.hi
        src/Lib.hs
        src/Lib.o
        test/Spec.hs
        ChangeLog.md
        LICENSE
        package.yaml
        projet.cabal
        README.md
        Setup.hs
        stack.yaml)
