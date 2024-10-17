#!/bin/bash -eux

# Should be executed from the root-directory

BRANCH_NAME=$(git branch --show-current)

if [ "$BRANCH_NAME" != "master" ]; then
  echo >&2 "Refusing to do release on branch ${BRANCH_NAME} because it's not master"
  exit 1
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

echo "On a branch ($BRANCH_NAME) publishing next patch"
npm version patch --no-git-tag-version --force

npm run lint
npm run test
npm run build

npm publish

TAG_NAME="$(jq '.version' -r package.json)"

git add package.json package-lock.json docs
git commit -m "Released ${TAG_NAME}"
git tag -a -m "Published by Jenkins" "${TAG_NAME}"
git push origin master:master --follow-tags
git push origin master:docs

# Advance to next alpha
npm version prepatch --preid=alpha --no-git-tag-version

git add package.json package-lock.json
git commit -m "Bumped to next development version"
git push origin master:master
