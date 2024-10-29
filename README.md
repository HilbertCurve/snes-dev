## snes-dev
Project following guidance of [this](https://georgjz.github.io/snesaa01/) tutorial. Thanks!

## building and running
My project uses make for compiling and testing because its straightforward to use with the ca65 compiler. To run, simply do:
```sh
make
bsnes {OUT_NAME}.smc
```
where OUT_NAME is the name of the SNES cartridge that was generated by make.