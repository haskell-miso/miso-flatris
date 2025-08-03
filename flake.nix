{

  inputs = {
    miso.url = "github:dmjio/miso";
  };

  outputs = inputs: 
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system: {
      devShells.wasm = inputs.miso.outputs.devShells.${system}.wasm;
      devShell = inputs.miso.outputs.devShells.${system}.default;
    });

}

