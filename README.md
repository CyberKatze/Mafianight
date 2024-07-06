## Setup for System
1. Install Nix: [installation-guide](https://nixos.org/download/#nix-install-linux)
2. Install Direnv:
   ```sh
   nix-env -i direnv
   # or
   nix-env -iA nixpkgs.direnv
   ```
3. Ensure Docker Compose is installed.

## Development
1. Go to the project directory:
   ```sh
   cd haskell-web
   ```
2. Allow Direnv:
   ```sh
   direnv allow
   ```

### Run in Development Mode
- Clean the project:
  ```sh
  make clean
  ```
- Prepare both Backend and Frontend:
  ```sh
  make
  ```
  This runs `make pre-back` and `make pre-front`, so you can skip these in the next sections.

#### Run Backend
1. Build and prepare build files:
   ```sh
   make pre-back
   ```
2. Run Backend:
   ```sh
   make dev-back
   ```
   > Make sure you are in the root directory of the project.

#### Run Frontend
1. Prepare and install node packages:
   ```sh
   make pre-front
   ```
2. Run Frontend:
   ```sh
   make dev-front
   ```
   > Make sure you are in the root directory of the project.

As your code changes, your site will be automatically recompiled and redeployed to localhost.

> If the server gets stuck, run `stack build`.

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional details.

### Tests
- Test Backend:
  ```sh
  make test-back
  ```
- Test Frontend:
  ```sh
  make test-front
  ```

### Makefile Help
You can use the following command to get help on the make targets:
```sh
make help
```
This will display descriptions for all the available make targets.

## Documentation
- Read the [Yesod Book](https://www.yesodweb.com/book) online for free.
- Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Your LTS version is in your `stack.yaml` file.
- For local documentation:
  - Generate and open Haddock documentation:
    ```sh
    stack haddock --open
    ```
  - Generate a Hoogle database and search for your query:
    ```sh
    stack hoogle <function, module or type signature>
    ```
- The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs.

## Getting Help
- Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell).
- Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb).
- Join chatrooms for help:
  - For IRC, try Freenode#yesod and Freenode#haskell.
  - [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
