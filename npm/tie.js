const { spawn } = require("child_process");
const cTable = require("console.table");
const fs = require("fs");
const { https } = require("follow-redirects");
const os = require("os");
const { join } = require("path");
const { version } = require("./package.json");

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

const installBinary = () => {
  return new Promise((resolve) => {
    console.log(`Downloading TIE v${version} for ${type}...`);
    if (type == "Linux") {
      fs.mkdirSync(join(__dirname, "bin"));
      installLinuxBinary().then(() => resolve());
    } else if (type == "Windows") {
      fs.mkdirSync(join(__dirname, "bin"));
      installWindowsBinary().then(() => resolve());
    } else {
      error("Could not find a binary for target platform.");
    }
  });
};

const linuxBinaryPath = join(__dirname, "bin", "tie");

const installLinuxBinary = () => {
  return new Promise((resolve) => {
    const f = fs.createWriteStream(linuxBinaryPath, { mode: 0o755 });
    https
      .get(
        `https://github.com/notquiteamonad/tie/releases/download/${version}/tie-${version}-linux-x64`,
        (res) => {
          if (res.statusCode == 200) {
            res.pipe(f);
            f.on("finish", () => {
              f.close(resolve);
            });
          } else {
            error("Could not download TIE executable.");
          }
        },
      )
      .on("error", () => {
        error("Could not download TIE executable.");
      });
  });
};

function runTIE() {
  const tie = spawn(linuxBinaryPath, process.argv.slice(2), {
    shell: true,
    stdio: "inherit",
  });
  tie.on("close", (code) => {
    process.exit(code);
  });
}

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

fs.access(join(__dirname, "bin"), fs.constants.F_OK, (doesNotExist) => {
  if (doesNotExist) {
    installBinary(type).then(() => runTIE());
  } else {
    runTIE();
  }
});

