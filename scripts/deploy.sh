#!/bin/sh

if [ $# -ne 2 ]; then
  echo "Failed Deploy: deploy.sh <tag> <os>"
  exit 1
fi

TAG="${1}"
OS="${2}"

echo "Deploying ${TAG} no ${OS}"
github-release upload \
  --token "${GITHUB_TOKEN}" \
  --owner 'jecxjo' \
  --repo 'todo.hs' \
  --tag "${TAG}" \
  --file "$(stack path --local-install-root)/bin/todo" \
  --name "todo-${OS}-${tag}"
