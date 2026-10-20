#!/usr/bin/env bash
# Add the Writing index to hypertidy.org (mdsumner/quarto-blog).
# Run from the root of a clean checkout of mdsumner/quarto-blog.
# Adds: catalogue.yml, writing.qmd (navbar), families.qmd, workbench.qmd (unlinked)
# and inserts a "Writing" navbar entry before Packages in _quarto.yml.
set -euo pipefail
test -f _quarto.yml && grep -q 'href: packages.qmd' _quarto.yml || { echo "run from quarto-blog root"; exit 1; }
git checkout -b writing-index

cat > catalogue.yml <<'EOF'
# hypertidy.org catalogue: one entry per work, wherever it lives.
# status: active | settled | historical are listed on /writing.html
#         seed | drafting are listed only on /workbench.html (not in the navbar)
# family: grids | projections | mesh | virtual-arrays | allonboard |
#         tracking | software-development | teaching

## grids
- title: "Regular in what?"
  path: https://hypertidy.github.io/regular-in-what/
  source: https://github.com/hypertidy/regular-in-what
  description: "Grids, coordinates, and the curvilinear confusion: a short book"
  family: grids
  kind: book
  status: active
  date: 2026-10-01
  categories: [grids, book]

- title: "Spatial data, underneath (a-book)"
  path: https://github.com/mdsumner/a-book
  source: https://github.com/mdsumner/a-book
  description: "Working draft of a book on grids, CRS, GDAL and geometry, from R and Python together"
  family: grids
  kind: book
  status: drafting
  date: 2026-10-01
  categories: [grids, book]

- title: "Cassini-Soldner far from the meridian"
  path: https://github.com/mdsumner/projcassini
  source: https://github.com/mdsumner/projcassini
  description: "PROJ's ellipsoidal +proj=cass goes wrong far from the central meridian"
  family: grids
  kind: note
  status: settled
  date: 2026-01-23
  categories: [grids, note]

## projections
- title: "playproj"
  path: https://github.com/hypertidy/playproj
  source: https://github.com/hypertidy/playproj
  description: "Map projections drawn the way they are built: a light, a sphere and a plane"
  family: projections
  kind: interactive
  status: active
  date: 2026-10-19
  categories: [projections, interactive]

- title: "Polar mapping challenges in R"
  path: https://mdsumner.github.io/polar-mapping-oghub/
  source: https://github.com/mdsumner/polar-mapping-oghub
  description: "Course book for OpenGeoHub 2019 (also mdsumner/projections-software)"
  family: projections
  kind: teaching
  status: historical
  date: 2019-08-29
  categories: [projections, teaching]

## mesh
- title: "Constrained triangulation in R"
  path: https://mdsumner.github.io/trianglewins/
  source: https://github.com/mdsumner/trianglewins
  description: "Survey of R triangulation packages, triangulation in context, and a benchmark harness"
  family: mesh
  kind: survey
  status: active
  date: 2026-10-01
  categories: [mesh, survey]

- title: "geoarrow triangles"
  path: https://github.com/mdsumner/trianglewins/tree/main/survey/dev-drafts
  source: https://github.com/mdsumner/trianglewins
  description: "Proposal draft for triangles in geoarrow, with UGRID/Zarr notes"
  family: mesh
  kind: draft
  status: seed
  date: 2026-10-01
  categories: [mesh, draft]

- title: "geomesh"
  path: https://github.com/mdsumner/geomesh
  source: https://github.com/mdsumner/geomesh
  description: "Partial tutorial on meshes for geospatial data"
  family: mesh
  kind: teaching
  status: historical
  date: 2018-11-21
  categories: [mesh, teaching]

## virtual-arrays
- title: "sidebyside"
  path: https://mdsumner.github.io/sidebyside/
  source: https://github.com/mdsumner/sidebyside
  description: "Two stances on browser-native raster and grid viewing, illustrated"
  family: virtual-arrays
  kind: essay
  status: active
  date: 2026-10-15
  categories: [virtual-arrays, essay]

- title: "recipes"
  path: https://mdsumner.github.io/recipes/
  source: https://github.com/mdsumner/recipes
  description: "Short recipes, one per file: ingredients, method, sources"
  family: virtual-arrays
  kind: collection
  status: active
  date: 2026-09-25
  categories: [virtual-arrays, collection]

- title: "virtualized"
  path: https://github.com/mdsumner/virtualized
  source: https://github.com/mdsumner/virtualized
  description: "Array virtualization in GDAL compared with xarray/VirtualiZarr"
  family: virtual-arrays
  kind: essay
  status: active
  date: 2026-06-11
  categories: [virtual-arrays, essay]

- title: "zarr-icechunk-assure"
  path: https://github.com/mdsumner/zarr-icechunk-assure
  source: https://github.com/mdsumner/zarr-icechunk-assure
  description: "File listing to versioned Icechunk repository, with assurance checks"
  family: virtual-arrays
  kind: essay
  status: active
  date: 2026-07-08
  categories: [virtual-arrays, essay]

- title: "rumi vs GeoTIFF: decode benchmark"
  path: https://github.com/mdsumner/rumi
  source: https://github.com/mdsumner/rumi
  description: "Splitting rumi's speed-up over GeoTIFF into its parts"
  family: virtual-arrays
  kind: benchmark
  status: settled
  date: 2026-09-23
  categories: [virtual-arrays, benchmark]

- title: "zarrquet"
  path: https://github.com/mdsumner/zarrquet
  source: https://github.com/mdsumner/zarrquet
  description: "Virtual Zarr references as Parquet, worked example"
  family: virtual-arrays
  kind: draft
  status: seed
  date: 2025-05-07
  categories: [virtual-arrays, draft]

- title: "vzarr-examples"
  path: https://github.com/mdsumner/vzarr-examples
  source: https://github.com/mdsumner/vzarr-examples
  description: "Virtual Zarr examples (ESA CCI in Parquet)"
  family: virtual-arrays
  kind: draft
  status: seed
  date: 2025-04-05
  categories: [virtual-arrays, draft]

## allonboard
- title: "allonboard"
  path: https://allboa.github.io/
  source: https://github.com/allboa/allboa.github.io
  description: "Landing page for the allonboard project: map scenes in their own CRS"
  family: allonboard
  kind: hub
  status: active
  date: 2026-10-06
  categories: [allonboard, hub]

- title: "allonboard design record"
  path: https://github.com/allboa/design
  source: https://github.com/allboa/design
  description: "Origin record, decision records and the org-level brief"
  family: allonboard
  kind: design
  status: active
  date: 2026-10-06
  categories: [allonboard, design]

- title: "scenespec"
  path: https://github.com/allboa/scenespec
  source: https://github.com/allboa/scenespec
  description: "A small renderer-neutral JSON document that describes a map scene"
  family: allonboard
  kind: design
  status: active
  date: 2026-10-06
  categories: [allonboard, design]

- title: "allonboard spikes"
  path: https://github.com/allboa/spikes
  source: https://github.com/allboa/spikes
  description: "Throwaway experiments, each ending in a decision record"
  family: allonboard
  kind: draft
  status: drafting
  date: 2026-09-30
  categories: [allonboard, draft]

## tracking
- title: "Trackage"
  path: https://trackage.r-universe.dev/
  source: https://github.com/Trackage
  description: "Animal tracking packages: trip, tripEstimation, extent.r"
  family: tracking
  kind: hub
  status: settled
  date: 2026-02-01
  categories: [tracking, hub]

## software-development
- title: "wxylib"
  path: https://wxylib.org/docs/
  source: https://github.com/wxylib/docs
  description: "Rust crates that give GDAL's modular ideas their own homes: docs, how-tos, explainers"
  family: software-development
  kind: hub
  status: active
  date: 2026-10-19
  categories: [software-development, hub]

- title: "wxylib design"
  path: https://wxylib.org/docs/design/README.html
  source: https://github.com/wxylib/design
  description: "Design notes, surveys and staging plans, each ending in a gate"
  family: software-development
  kind: design
  status: active
  date: 2026-10-19
  categories: [software-development, design]

- title: "GDAL7"
  path: https://github.com/rgdal-dev/GDAL7
  source: https://github.com/rgdal-dev/GDAL7
  description: "GDAL's API modelled in R, generated from GDAL's SWIG definitions, bound with cpp11 and S7"
  family: software-development
  kind: package
  status: active
  date: 2026-10-06
  categories: [software-development, package]

- title: "agdl"
  path: https://github.com/rgdal-dev/agdl
  source: https://github.com/rgdal-dev/agdl
  description: "TODO: one line"
  family: software-development
  kind: package
  status: drafting
  date: 2026-10-01
  categories: [software-development, package]

- title: "mailib"
  path: https://github.com/mailib
  source: https://github.com/mailib
  description: "TODO: one line"
  family: software-development
  kind: hub
  status: drafting
  date: 2026-10-01
  categories: [software-development, hub]

- title: "gdal-r-python"
  path: https://github.com/mdsumner/gdal-r-python
  source: https://github.com/mdsumner/gdal-r-python
  description: "Notebook of findings: GDAL dev notes, mdim chunk references, virtual TIFF, arrow"
  family: software-development
  kind: collection
  status: active
  date: 2026-10-12
  categories: [software-development, collection]

- title: "ggsql spatial"
  path: https://mdsumner.github.io/ggsql-spatial-demo/
  source: https://github.com/mdsumner/ggsql-spatial-demo
  description: "Exploring spatial support in ggsql, with an illustrated guide"
  family: software-development
  kind: essay
  status: active
  date: 2026-10-13
  categories: [software-development, essay]

## teaching
- title: "Talks since 2014"
  path: https://github.com/mdsumner/talks
  source: https://github.com/mdsumner/talks
  description: "Slides, figures and data snippets, through a 2026 virtual Zarr talk"
  family: teaching
  kind: talk
  status: settled
  date: 2026-06-11
  categories: [teaching, talk]

- title: "Geospatial computing and graphics (OpenGeoHub)"
  path: https://github.com/mdsumner/geo-comp-graphics-oghub
  source: https://github.com/mdsumner/geo-comp-graphics-oghub
  description: "Course material for OpenGeoHub"
  family: teaching
  kind: teaching
  status: historical
  date: 2019-10-02
  categories: [teaching]

- title: "targets: discovered knowledge"
  path: https://github.com/mdsumner/targeted-learning
  source: https://github.com/mdsumner/targeted-learning
  description: "Unofficial companion to the targets manual: the why behind its features"
  family: teaching
  kind: essay
  status: settled
  date: 2025-12-03
  categories: [teaching, essay]
EOF

cat > writing.qmd <<'EOF'
---
title: "Writing"
description: "Books, surveys, essays and interactive pieces, wherever they live."
listing:
  id: writing
  contents: catalogue.yml
  type: table
  sort: "date desc"
  fields: [date, title, description, family, kind]
  field-display-names:
    family: "Family"
    kind: "Kind"
  date-format: "YYYY-MM"
  categories: true
  sort-ui: true
  filter-ui: true
  exclude:
    status: "{seed,drafting}"
---

Longer pieces and project families gathered in one place. Each entry links to
where the work lives; the blog posts are on the [home page](/index.html).
See also [families](/families.html).

::: {#writing}
:::
EOF

cat > workbench.qmd <<'EOF'
---
title: "Workbench"
description: "Seeds and drafts in progress."
listing:
  id: workbench
  contents: catalogue.yml
  type: table
  sort: "date desc"
  fields: [date, title, description, family, status]
  field-display-names:
    family: "Family"
    status: "Status"
  date-format: "YYYY-MM"
  include:
    status: "{seed,drafting}"
---

Work in progress, not linked from the navbar.

::: {#workbench}
:::
EOF

cat > families.qmd <<'EOF'
---
title: "Families"
description: "Project families: writing, packages and pilots that belong together."
listing:
  - id: grids
    contents: catalogue.yml
    type: table
    sort: "date desc"
    fields: [date, title, description, kind]
    date-format: "YYYY-MM"
    include:
      family: "grids"
    exclude:
      status: "{seed,drafting}"
  - id: projections
    contents: catalogue.yml
    type: table
    sort: "date desc"
    fields: [date, title, description, kind]
    date-format: "YYYY-MM"
    include:
      family: "projections"
    exclude:
      status: "{seed,drafting}"
  - id: mesh
    contents: catalogue.yml
    type: table
    sort: "date desc"
    fields: [date, title, description, kind]
    date-format: "YYYY-MM"
    include:
      family: "mesh"
    exclude:
      status: "{seed,drafting}"
  - id: virtual-arrays
    contents: catalogue.yml
    type: table
    sort: "date desc"
    fields: [date, title, description, kind]
    date-format: "YYYY-MM"
    include:
      family: "virtual-arrays"
    exclude:
      status: "{seed,drafting}"
  - id: allonboard
    contents: catalogue.yml
    type: table
    sort: "date desc"
    fields: [date, title, description, kind]
    date-format: "YYYY-MM"
    include:
      family: "allonboard"
    exclude:
      status: "{seed,drafting}"
  - id: tracking
    contents: catalogue.yml
    type: table
    sort: "date desc"
    fields: [date, title, description, kind]
    date-format: "YYYY-MM"
    include:
      family: "tracking"
    exclude:
      status: "{seed,drafting}"
  - id: software-development
    contents: catalogue.yml
    type: table
    sort: "date desc"
    fields: [date, title, description, kind]
    date-format: "YYYY-MM"
    include:
      family: "software-development"
    exclude:
      status: "{seed,drafting}"
  - id: teaching
    contents: catalogue.yml
    type: table
    sort: "date desc"
    fields: [date, title, description, kind]
    date-format: "YYYY-MM"
    include:
      family: "teaching"
    exclude:
      status: "{seed,drafting}"
---

Drafts in each family are on the [workbench](/workbench.html).

## Grids

Regular, rectilinear and curvilinear grids, coordinates and their alignment.

::: {#grids}
:::

## Projections

How map projections are built, drawn and taught.

::: {#projections}
:::

## Mesh

Triangulation and meshes for geospatial data.

::: {#mesh}
:::

## Virtual arrays

Virtual Zarr, Icechunk, GDAL multidim and browser-native grids.

::: {#virtual-arrays}
:::

## allonboard

Map scenes in their own coordinate reference system.

::: {#allonboard}
:::

## Tracking

Animal tracking data and estimation.

::: {#tracking}
:::

## Software development

Bindings and libraries in development: wxylib, mailib, GDAL7, agdl.

::: {#software-development}
:::

## Teaching and talks

Courses, tutorials and talks.

::: {#teaching}
:::
EOF

# navbar: Writing before Packages (idempotent)
if ! grep -q 'href: writing.qmd' _quarto.yml; then
  python3 - <<'PY'
s = open("_quarto.yml").read()
old = "      - href: packages.qmd\n"
new = "      - href: writing.qmd\n        text: Writing\n" + old
assert old in s
open("_quarto.yml", "w").write(s.replace(old, new, 1))
PY
fi

quarto preview writing.qmd   # check, then Ctrl-C
# git add catalogue.yml writing.qmd workbench.qmd families.qmd _quarto.yml
# git commit -m "Add Writing index: catalogue.yml with Writing, Families and Workbench listings"
