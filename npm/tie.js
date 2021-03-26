const { spawn } = require("child_process");
const cTable = require("console.table");
const fs = require("fs");
const { https } = require("follow-redirects");
const gunzip = require("gunzip-maybe");
const os = require("os");
const { join } = require("path");
const tar = require("tar-fs");
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
  {
    TYPE: "Windows_NT",
    ARCHITECTURE: "x64",
  },
];

const type = os.type();
const architecture = os.arch();

const binPath = join(__dirname, "bin");

const installBinary = () => {
  return new Promise((resolve) => {
    console.log(`Downloading TIE v${version} for ${type}...`);
    if (type === "Linux") {
      fs.mkdirSync(binPath, { recursive: true });
      installLinuxBinary().then(() => resolve());
    } else if (type == "Windows_NT") {
      fs.mkdirSync(binPath);
      installWindowsBinary().then(() => resolve());
    } else {
      error("Could not find a binary for target platform.");
    }
  });
};

const linuxBinaryPath = join(binPath, "tie");

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

const windowsBinaryPath = join(binPath, "tie-windows", "tie-windows.exe");

const installWindowsBinary = () => {
  return new Promise((resolve) => {
    https
      .get(
        `https://github.com/notquiteamonad/tie/releases/download/${version}/tie-${version}-windows-x64.tar.gz`,
        (res) => {
          if (res.statusCode == 200) {
            res.pipe(gunzip()).pipe(
              tar.extract(binPath, {
                dmode: 0o755,
                fmode: 0o755,
                finish: resolve,
              }),
            );
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
  const path = type === "Linux"
    ? linuxBinaryPath
    : (type === "Windows_NT"
        ? windowsBinaryPath
        : error("Can't find TIE binary path. Unsupported OS?")
      );
  const tie = spawn(path, process.argv.slice(2), {
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
    )}\nStatus of support for Mac is available at the following URL: https://github.com/notquiteamonad/tie/issues/27`,
  );
}

fs.access(binPath, fs.constants.F_OK, (doesNotExist) => {
  if (doesNotExist) {
      installBinary(type).then(() => runTIE());
  } else {
      runTIE();
  }
});
