# Variables
FRONTEND_DIR = client
BACKEND_DIR = server
REG_URL = registery.mafianight.me

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

.PHONY: ci-test-back ci-build-front ci-build-back ci-deploy-back

ci-test-unit-front: ## Runs the frontend unit tests on CI
	cd $(FRONTEND_DIR) && $(PNPM_TEST) --project=unit

ci-test-e2e-front: ## Runs the frontend end-to-end tests on CI
	cd $(FRONTEND_DIR) && $(PNPM_TEST) --project=e2e

ci-test-back: ## Runs the backend tests on CI
	cd $(BACKEND_DIR) && \
	stack test

ci-build-front: ## Builds the frontend on CI
	cd $(FRONTEND_DIR) && \
	VITE_API_URL=$(VITE_API_URL) $(PNPM_RUN_BUILD)


ci-build-back: ## Builds the backend on CI
	nix build .#backendImage -o backendImage && \
	docker load < backendImage 

push-docker-back: ## Pushes the backend image to Registery
	docker login $(REG_URL) && \
	docker tag backend:latest $(REG_URL)/backend:$(VERSION) && \
	docker tag backend:latest $(REG_URL)/backend:latest && \
	docker push $(REG_URL)/backend:$(VERSION) && \
	docker push $(REG_URL)/backend:latest


ci-deploy-back: ## deploys the backend on CI
	docker compose -f server/docker-compose.yml down -v && \
	docker compose -f server/docker-compose.yml pull app && docker compose -f server/docker-compose.yml up -d app 
	
ci-deploy-front: ## deploys the front on CI
	rsync -avz --delete $(FRONTEND_DIR)/dist/ /var/www/mafianight.me
