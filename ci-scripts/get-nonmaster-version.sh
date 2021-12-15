#!/bin/bash -eu

if [ -z "$1" ]; then
  echo >&2 "Missing first argument, expected branch name"
  exit 1
fi

if [ -z "$2" ]; then
  echo >&2 "Missing second argument, expected build number"
  exit 1
fi

BRANCH_NAME=$1
BUILD_NUMBER=$2

if [[ ! "$BUILD_NUMBER" =~ ^[0-9]+$ ]]; then
  echo >&2 "Second argument (build number) must be a number, it was: '$BUILD_NUMBER'"
  exit 1
fi

YN_BRANCH_RE="[Yy][Nn]-[1-9][0-9]*"
DEVELOP_BRANCH_RE="^develop$"
RELEASE_BRANCH_RE="^release$"

if [[ "$BRANCH_NAME" =~ $YN_BRANCH_RE ]]; then
  YN_NUMBER=$(echo $BRANCH_NAME | sed 's/.*[Yy][Nn]-\([1-9][0-9]*\).*/\1/')
  NPM_VERSION="0.0.$YN_NUMBER-alpha.$BUILD_NUMBER"
elif [[ "$BRANCH_NAME" =~ $DEVELOP_BRANCH_RE ]]; then
  NPM_VERSION="0.1.0-alpha.$BUILD_NUMBER"
elif [[ "$BRANCH_NAME" =~ $RELEASE_BRANCH_RE ]]; then
  NPM_VERSION="0.2.0-alpha.$BUILD_NUMBER"
else
  echo >&2 "$BRANCH_NAME not supported for publishing - exiting"
  exit 0
fi

export NPM_VERSION
