const { spawn } = require("child_process");
const cTable = require("console.table");
const os = require("os");
const { join } = require("path");

const error = (msg) => {
  console.error(msg);
  process.exit(1);
};

const supportedPlatforms = [
  {
    TYPE: "Linux",
    ARCHITECTURE: "x64",
  },
];

const type = os.type();
const architecture = os.arch();

let onSupportedPlatform = false;
for (let index in supportedPlatforms) {
  let supportedPlatform = supportedPlatforms[index];
  if (
    type === supportedPlatform.TYPE &&
    architecture === supportedPlatform.ARCHITECTURE
  ) {
    onSupportedPlatform = true;
  }
}

if (!onSupportedPlatform) {
  error(
    `Platform with type "${type}" and architecture "${architecture}" is not currently supported by TIE.\nYour system must be one of the following:\n\n${cTable.getTable(
      supportedPlatforms,
    )}\nStatus of support for Mac and Windows is available at the following URLS:\n  Mac: https://github.com/notquiteamonad/tie/issues/27\n  Windows: https://github.com/notquiteamonad/tie/issues/25`,
  );
}

const tie = spawn(join(__dirname, "bin", "tie"), process.argv.slice(2), {
  shell: true,
  stdio: "inherit",
});

tie.on("close", (code) => {
  process.exit(code);
});
