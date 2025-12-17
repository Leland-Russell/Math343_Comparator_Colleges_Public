# Shiny app bin
This is a "bin" format of the shiny app. This is not binary as the "bin" might otherwise suggest, but it uses precompiled data. Using precompiled data allows us to publicly share all of this while preserving private data, as only a few summary statistics remain from the private data.

More documentation on the methodology and how these data were compiled are in the [project report](https://hi) and the private data compilation version of this file. If you do not have the private data compilation tarball, then you cannot view this part of the data compilation.

To run the app, you should call

```bash
make run # or more simply
make
```

# Other functionality

If you'd like to change the data and re-compress the data, you can call

```bash
make compress
```

but first you should probably clear the compressed data; to do this call

```bash
rm compressed.tar.gz
```

To clear extra data, generally for sharing the compressed file, call 
```bash
make full_clean
```
