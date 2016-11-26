### TGAS + Hipparcos Catalog

The TGAS + Hipparcos Catalog (THC) combines two star catalogs. It contains
the 3D position, brightness, and color of over 2 million real stars.

TGAS means Tycho-Gaia Astrometric Solution. It refers to a catalog of over
2 million stars whose 3D positions have been released as part of [Gaia Data
Release #1](http://cosmos.esa.int/web/gaia/dr1).

[Hipparcos](https://en.wikipedia.org/wiki/Hipparcos) is a catalog of about
120,000 mostly bright stars. Since TGAS is missing most bright stars, the two
are combined to give a more complete picture.

To generate the file `thc.csv`, clone the repository and run these commands
from within the `thc` directory. The file has over 2 million rows, one for each
star.

```bash
sh download_tgas.sh
python convert_tgas_to_csv.py
python convert_hipparcos_to_csv.py
python merge_tgas_and_hipparcos.py
```
