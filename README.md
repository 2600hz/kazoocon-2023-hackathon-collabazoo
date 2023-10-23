# Collabazoo: KAZOOcon 2023 hackathon starter kit

Collabazoo is a simple app that allows you to draw on a whiteboard canvas. Other users can observe the drawings in real time. This project is part of the [starter kit for the KAZOOcon 2023 hackathon](https://github.com/2600hz/kazoocon-2023-hackathon-collabazoo).

The project is composed of 3 parts: this KAZOO application (kapp), an ugly [example web client](./priv/example-client.html), and a Meta Quest 2 VR application. When users draw in the VR world, the drawings are relayed through KAZOO's [blackhole](https://docs.2600hz.com/integrator/applications/blackhole/doc/) app in real time to the web client! The kapp then broadcasts the drawings to all connected clients, including the web client and the VR application.

## Hackathon instructions

Instructions and project ideas can be found [here](https://hz2600-my.sharepoint.com/:w:/g/personal/dfinke_2600hz_com/ETVxlUhfmaZEsPwHjSTsEY4BqqWUiIiZzAwISes4Osbn7A).

## Installation (kapp)

### From source

#### KAZOO 4.3

1. Clone this repository to the `applications` directory of your KAZOO source tree.
2. Run `make` in the `applications/collabazoo` directory.
3. Rebuild your KAZOO release, e.g. using `make build-dev-release` in the KAZOO source tree root.

#### KAZOO 5.x

Building from source is not available. If the [precompiled build for KAZOO 5.3](#kazoo-5x-1) does not work for you, please contact me (at the hackathon booth or [via email](mailto:dfinke@2600hz.com)) for a build targeting your KAZOO 5.x version. I will compile one for you.

You can try building the KAZOO 4.3 version and running it on KAZOO 5.x, but it is not guaranteed to work.

### Precompiled

Precompiled versions are available in the [releases](./releases) directory for KAZOO 4.3.142 and 5.3.65.

1. Extract the `.tar.gz` to `/opt/kazoo/lib`.

## Installation (web client)

1. Update the Crossbar/Blackhole hosts/ports near the top of the [example web client](./priv/example-client.html) to point to your KAZOO servers.
2. Serve the example client `.html` file from a web server. Alternatively, you can just open the file in your browser (`file:` protocol).
3. Visit the URL for the client in your browser.

## Installation/usage (VR app)

1. Download and extract the [app source code `.zip` file](https://hz2600-my.sharepoint.com/:u:/g/personal/dfinke_2600hz_com/EZTct-gR-TRPllMume5z7UAB-EHf47l74PvkLHLbREFuJA?e=VoaHzJ).
2. Follow the instructions in the included `README.md`.

## Usage (kapp)

1. Start or restart KAZOO after installation (e.g. `systemctl restart kazoo-applications`).
2. Start the kapp (e.g. `sup kapps_controller start_app collabazoo`) (only necessary if you will build on top of the kapp).
3. Start the blackhole module (e.g. `sup blackhole_maintenance start_module bh_collabazoo`).

## How it works (kapp)

The [Blackhole module](./src/integration/bh_collabazoo.erl) adds `collabazoo` Blackhole bindings that clients can subscribe to. It also accepts commands sent by Blackhole clients to draw on or clear the whiteboard. Those commands result in corresponding AMQP messages being published using the [KAPI module](./src/kapi_collabazoo.erl) to all listeners bound by the Blackhole bindings. Hence, every command is broadcast to all other collaborators (within the same account).

The full `collabazoo` kapp is only a formality right now, but it exists to be built on, e.g. for adding whiteboard state persistence.

## How it works (web client)

When the user submits their credentials, a `fetch` is made to the Crossbar [user authentication endpoint](https://docs.2600hz.com/supported/applications/crossbar/doc/user_authentication/) to acquire an auth token. The token is then used to authenticate with the Blackhole module. The Blackhole module then binds the client to the `collabazoo` bindings, which allows the client to receive messages from the kapp via WebSocket. `draw` and `clear` messages update the canvas on the web page accordingly. The client can also send a `clear` command to the Blackhole module, which will result in a `clear` message being broadcast to all other collaborators and their whiteboards being cleared.
