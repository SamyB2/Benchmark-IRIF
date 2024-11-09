#!/bin/sh

clean_workspace() {
  if [ -e generate ]; then
    rm generate
  fi
  cd systems_generation
  dune clean
  cd ..
  remove_files
  cd systems_generation
}

remove_files() {
  cd systems_generation
  rm -rf ./sag_files/*
  rm -rf ./uppaal_files/*
  rm -rf ./system_files/*
  rm -rf ../scrypt/*.info  
  cd ..
}

if [ "$1" = "build" ]; then
  clean_workspace
  dune build
  ln -sf systems_generation/_build/default/bin/main.exe ../generate
  ln -sf systems_generation/_build/default/bin/main2.exe ../translate
  cd ../rt_to_xta
  dune build
  echo "Build complete"
  cd ..
elif [ "$1" = "clean" ]; then
  clean_workspace
  cd ..
  echo "Clean complete"
elif [ "$1" = "clear" ]; then
  remove_files
  echo "Clean files complete"
else
  echo "Unknown command: $1"
  echo "Usage: $0 {build|clean}"
  exit 1
fi