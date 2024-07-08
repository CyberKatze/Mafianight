# Variables
FRONTEND_DIR = client
BACKEND_DIR = server

# Frontend commands
PNPM_RUN_BUILD = pnpm run build
PNPM_INSTALL = pnpm install
PNPM_DEV = pnpm run dev
PNPM_TEST = pnpm playwright test 

# Backend commands
STACK_BUILD = stack build
STACK_DEV = yesod devel
STACK_RUN = stack exec haskell-web

# Targets
.PHONY: all build-front build-back clean dev-front dev-back run-db test-front test-back help

all: pre-front pre-back ## Prepares both frontend and backend

pre-front: $(FRONTEND_DIR)/node_modules ## Prepares the frontend by installing dependencies

$(FRONTEND_DIR)/node_modules: $(FRONTEND_DIR)/package.json $(FRONTEND_DIR)/pnpm-lock.yaml
	cd $(FRONTEND_DIR) && $(PNPM_INSTALL)

pre-back: $(BACKEND_DIR)/.stack-work ## Prepares the backend by building the project

$(BACKEND_DIR)/.stack-work: $(BACKEND_DIR)/package.yaml $(BACKEND_DIR)/stack.yaml
	cd $(BACKEND_DIR) && $(STACK_BUILD)

clean: ## Cleans up generated files and stops Docker containers
	rm -rf $(FRONTEND_DIR)/node_modules $(FRONTEND_DIR)/dist $(BACKEND_DIR)/.stack-work \
	 &&	cd $(BACKEND_DIR) && docker compose down -v

dev-front: ## Runs the frontend in development mode
	cd $(FRONTEND_DIR) && $(PNPM_DEV)

dev-back: run-db ## Runs the backend in development mode
	cd $(BACKEND_DIR) && docker compose up -d db && $(STACK_DEV)

test-front: ## Runs the frontend tests
	cd $(FRONTEND_DIR) && $(PNPM_TEST)

test-back: ## Runs the backend tests
	cd $(BACKEND_DIR) && \
	docker compose up -d db-test && \
	stack test --flag haskell-web:library-only --flag haskell-web:dev


help: ## Displays this help message
	@echo "Usage: make [TARGET]"
	@echo ""
	@echo "Targets:"
	@awk 'BEGIN {FS = ":.*?## "}; /^[a-zA-Z_-]+:.*?## / { printf "  %-20s %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

.PHONY: help

ci-test-back: ## Runs the backend tests on CI
	cd $(BACKEND_DIR) && \
	stack test

