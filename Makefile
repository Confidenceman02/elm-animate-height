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
