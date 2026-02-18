# leios-wfa-ls-demo
A demo that showcases wFA^LS, the core of Leios committee selection (see [this](https://doi.org/10.1145/3576915.3623194) paper)

## Setup
To run this demo, you need to bootstrap a node on any testnet. The steps below can help with this.
```bash
cd /tmp
mkdir preview
cd preview
wget https://book.world.dev.cardano.org/environments/preview/config.json
wget https://book.world.dev.cardano.org/environments/preview/topology.json
wget https://book.world.dev.cardano.org/environments/preview/peer-snapshot.json
wget https://book.world.dev.cardano.org/environments/preview/byron-genesis.json
wget https://book.world.dev.cardano.org/environments/preview/shelley-genesis.json
wget https://book.world.dev.cardano.org/environments/preview/alonzo-genesis.json
wget https://book.world.dev.cardano.org/environments/preview/conway-genesis.json
```
Then run
```bash
nix run github:IntersectMBO/cardano-node/10.5.3#cardano-node -- run +RTS -qg -qb -RTS \
 --topology /tmp/preview/topology.json \
 --database-path /tmp/preview/db \
 --socket-path /tmp/preview/node.socket \
 --host-addr 0.0.0.0 \
 --port 6030 \
 --config /tmp/preview/config.json
```
which lets you query, for example, via
```bash
export CARDANO_NODE_SOCKET_PATH=/tmp/preview/node.socket
nix run github:IntersectMBO/cardano-node/10.5.3#cardano-cli -- query tip --testnet-magic 2
```
## Run the demo
Given that the above sets up the preview testnet, we can run
```bash
nix run .#leios-wfa-ls-demo-exe -- --network-magic 2 --socket-path /tmp/preview/node.socket
```
