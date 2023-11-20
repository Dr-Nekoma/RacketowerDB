const net = require("node:net");
const { argv } = require("node:process");

const PORT = 8891;

if (argv[2]) {
    const socket = net.createConnection(PORT, "localhost", () => {
        socket.write(argv[2], "utf8");
    });
}
