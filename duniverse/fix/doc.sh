#!/bin/bash
set -euo pipefail
shopt -s extglob

# This script helps make sense of the error messages produced by odoc.

while read line
do
  buffer="$line"
  case "$line" in
  File*:)
    read line
    buffer="$buffer$line"
    case "$line" in

    "Warning: Couldn't find the following modules:")
      read line
      buffer="$buffer$line"
      case "$line" in
      *([:blank:])Stdlib)
        # A warning that Stdlib could not be found. Skip.
        echo "Skipping warnings about Stdlib."
        ;;
      *)
        # A warning that some other modules could not be found. Echo.
        echo "$buffer"
        ;;
      esac
      ;;

    "Warning: Failed to lookup type unresolvedroot(Stdlib)"*)
      # A warning about Stdlib. Skip.
      echo "Skipping warnings about Stdlib."
      ;;

    "Warning: While"*)
      read line
      buffer="$buffer$line"
      case "$line" in
      "Failed to lookup type unresolvedroot(Stdlib)"*)
        # A multi-line warning about Stdlib. Skip.
        echo "Skipping warnings about Stdlib."
        ;;
      *)
        # Another kind of warning. Echo.
        echo "$buffer"
        ;;
      esac
      ;;

    *)
      # Another kind of warning. Echo.
      echo "$buffer"
      ;;
    esac
    ;;

  *)
    echo "$buffer"
    ;;
  esac
done
