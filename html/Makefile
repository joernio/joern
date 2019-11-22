.PHONY: bootstrap build-dev build-prod clean clean-ci lint lint-ci release release-ci test test-ci

bootstrap:
	npx lerna bootstrap --hoist --force-local

bootstrap-ci:
	npx lerna bootstrap --ci

build-dev:
	npx lerna run build-dev

build-prod:
	npx lerna run build-prod

clean:
	npx lerna clean

clean-ci:
	npx lerna clean --yes

release: clean-ci bootstrap build-prod
	npx lerna publish --registry $(NPM_REGISTRY)

release-ci: clean-ci bootstrap-ci build-prod
	npx lerna publish --yes --registry $(NPM_REGISTRY)

lint:
	npx lerna run lint

lint-ci:
	npx lerna run lint --yes

test:
	npx lerna run test

test-ci:
	npx lerna run test --yes

check-outdated:
	npx lerna exec --no-bail "npx npm-check-updates"
	npx npm-check-updates

update-deps:
	npx npm-check-updates -u && npm i
	npx lerna exec --no-bail "npx npm-check-updates -u && npm i && npm audit fix"
	make bootstrap
	npx lerna exec --no-bail "npm i --no-save"