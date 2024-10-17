#!/bin/bash -eux

# Should be executed from the root-directory

if git describe --exact-match HEAD 2>/dev/null; then
  echo "This is a tag - release job already published that so nothing to do."
  exit
fi

export N_PREFIX="${WORKSPACE}/n"
PATH="$(pwd)/node_modules/.bin:${N_PREFIX}/bin:${PATH}"
export PATH

n lts

which node
which npm

node --version

echo "Logged in to npm as '$(npm whoami)'"

npm ci

BRANCH_NAME="$(git rev-parse --abbrev-ref HEAD)"
#BRANCH_NAME=$(git branch --show-current)
if [ "$BRANCH_NAME" = "master" ]; then
  # Latest in package.json is the next-version with an alpha-mark
  # so just advance to patch for easier parsing
  npm version patch --no-git-tag-version --force

  NEXT_VERSION_STRING=$(grep '"version":' package.json)
  NEXT_VERSION=$(echo "$NEXT_VERSION_STRING" | sed 's/.*\"version\":\ *\"\(.*\)\".*/\1/')
  NPM_VERSION="${NEXT_VERSION}-alpha.${BUILD_NUMBER}"
  export NPM_VERSION
else
  source ci-scripts/get-nonmaster-version.sh "$BRANCH_NAME" "$BUILD_NUMBER"
fi

echo "On a branch ($BRANCH_NAME) publishing version $NPM_VERSION"
npm version "${NPM_VERSION}" --no-git-tag-version --force

npm run lint
npm run test
npm run build

npm publish
