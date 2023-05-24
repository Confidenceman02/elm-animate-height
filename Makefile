.PHONY: install
install: .yarn.INSTALLED
.yarn.INSTALLED: package.json yarn.lock
	yarn install
	@touch $@

.PHONY: elm-reactor
elm-reactor:
	cd examples && elm reactor

.PHONY: preview-docs
preview-docs: install
	yarn elm-doc-preview

.PHONY: e2e-test
e2e-test: install
	yarn mocha --timeout 10000 --exit -r ts-node/register 'tests/**/*.spec.ts'

.PHONY: ci-e2e-test
ci-e2e-test: install
	make elm-reactor & make e2e-test
