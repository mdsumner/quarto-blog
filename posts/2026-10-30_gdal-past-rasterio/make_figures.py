"""Figures for the appendix of "GDAL past the rasterio view".

Run with GDAL >= 3.11 (osgeo), numpy and matplotlib:

    python make_figures.py fixtures

Inputs: the oisst-like Zarr store and the curvilinear swath from
github.com/wxylib/fixtures; everything else is made here. Writes fig1.png .. fig6.png.
"""

import sys
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from osgeo import gdal

gdal.UseExceptions()
FIX = sys.argv[1] if len(sys.argv) > 1 else "fixtures"
DPI = 110


def mem(arr, gt, crs, dtype=gdal.GDT_Float32, nodata=None):
    d = gdal.GetDriverByName("MEM").Create("", arr.shape[1], arr.shape[0], 1, dtype)
    d.SetGeoTransform(gt)
    d.SetProjection(crs)
    b = d.GetRasterBand(1)
    b.WriteArray(arr)
    if nodata is not None:
        b.SetNoDataValue(nodata)
    return d


def checkerboard(step=0.5, wave=0.35):
    """Global lon/lat grid, row 0 at the north edge: 15-degree checks plus a wave."""
    lon = np.arange(-180 + step / 2, 180, step)
    lat = np.arange(90 - step / 2, -90, -step)
    lo, la = np.meshgrid(lon, lat)
    z = (np.floor(la / 15) + np.floor(lo / 15)) % 2
    z = z + wave * np.sin(lo * np.pi / 30) * np.cos(la * np.pi / 30)
    return z.astype("float32"), (-180.0, step, 0.0, 90.0, 0.0, -step)


def canvas(crs, cx, cy, width_m, nx=400, ny=400):
    h = width_m * ny / nx
    return dict(dstSRS=crs, width=nx, height=ny,
                outputBounds=(cx - width_m / 2, cy - h / 2, cx + width_m / 2, cy + h / 2))


def panel(ax, a, title, **kw):
    ax.imshow(a, interpolation="nearest", **kw)
    ax.set_title(title, fontsize=10)
    ax.set_xticks([])
    ax.set_yticks([])


# ---------------------------------------------------------------- figure 1
def fig1():
    store = f"{FIX}/cf/oisst-like.zarr"
    import xarray as xr
    rg = gdal.OpenEx(store, gdal.OF_MULTIDIM_RASTER).GetRootGroup()
    raw = rg.OpenMDArray("sst")
    att = {a.GetName(): a.Read() for a in raw.GetAttributes()}
    xr.Dataset({"sst": (("lat", "lon"), raw.GetView("[0,0,:,:]").ReadAsArray(),
                        {k: att[k] for k in ("scale_factor", "add_offset") if k in att})},
               coords={"lat": rg.OpenMDArray("lat").ReadAsArray(), "lon": rg.OpenMDArray("lon").ReadAsArray()}
               ).to_netcdf("oisst-copy.nc", encoding={"sst": {"_FillValue": raw.GetNoDataValueAsDouble()}})
    z = gdal.Open(f'ZARR:"{store}":/sst')
    n = gdal.Open('NETCDF:"oisst-copy.nc":sst')
    arr = gdal.OpenEx(store, gdal.OF_MULTIDIM_RASTER).GetRootGroup().OpenMDArray("sst")
    u = arr.GetView("[0,0,:,:]").GetUnscaled().ReadAsArray()
    fig, ax = plt.subplots(1, 3, figsize=(13.5, 3.4), dpi=DPI)
    gz, gn = z.GetGeoTransform(), n.GetGeoTransform()
    panel(ax[0], z.GetRasterBand(1).ReadAsArray(), f"Zarr driver, classic: raw int16\ngt y0={gz[3]:g}, dy={gz[5]:+g} (south-up, not flipped)", cmap="viridis")
    panel(ax[1], n.GetRasterBand(1).ReadAsArray(), f"same data copied to netCDF, classic: raw int16\ngt y0={gn[3]:g}, dy={gn[5]:+g} (rows flipped to north-up)", cmap="viridis")
    m = np.ma.masked_invalid(u)
    ax[2].imshow(m, origin="lower", extent=(0, 360, -90, 90), cmap="viridis", interpolation="nearest")
    ax[2].set_title("multidim GetView + GetUnscaled: decoded,\nfile order, drawn with origin='lower'", fontsize=10)
    ax[2].set_xlabel("longitude (0 to 360)")
    ax[2].set_ylabel("latitude")
    fig.suptitle("Figure A1. One OISST-like store, three readings. imshow puts row 0 at the top, so a south-up array draws upside down.", fontsize=10, y=1.03)
    return fig


# ---------------------------------------------------------------- figure 2
def fig2():
    x0, dx, y0, dy, nx, ny = 145.0, 1.0, -40.0, -1.0, 4, 3
    fig, ax = plt.subplots(figsize=(6.4, 4.4), dpi=DPI)
    for i in range(nx + 1):
        ax.plot([x0 + i * dx] * 2, [y0, y0 + ny * dy], color="0.6", lw=1)
    for j in range(ny + 1):
        ax.plot([x0, x0 + nx * dx], [y0 + j * dy] * 2, color="0.6", lw=1)
    cx = x0 + (np.arange(nx) + 0.5) * dx
    cy = y0 + (np.arange(ny) + 0.5) * dy
    gx, gy = np.meshgrid(cx, cy)
    ax.plot(gx, gy, "o", color="tab:blue", ms=5)
    ax.plot([x0], [y0], "s", color="tab:red", ms=9, clip_on=False)
    ax.annotate("geotransform (x0, y0) = (145, -40)\nouter corner of cell (0, 0)", (x0, y0),
                xytext=(x0 + 1.0, y0 + 0.75), color="tab:red", fontsize=9,
                arrowprops=dict(arrowstyle="->", color="tab:red"))
    ax.annotate("xarray lon[0], lat[0] = (145.5, -40.5)\ncell centre", (cx[0], cy[0]),
                xytext=(x0 + 1.2, y0 - 1.15), color="tab:blue", fontsize=9,
                arrowprops=dict(arrowstyle="->", color="tab:blue"))
    ax.annotate("", (x0, y0 - 3.25), (x0 + 0.5, y0 - 3.25), arrowprops=dict(arrowstyle="<->"))
    ax.text(x0 + 0.25, y0 - 3.45, "dx/2", ha="center", fontsize=9)
    ax.set_xlim(144.6, 149.4)
    ax.set_ylim(-43.7, -38.4)
    ax.set_aspect("equal")
    ax.set_xlabel("x (longitude)")
    ax.set_ylabel("y (latitude)")
    ax.set_title("Figure A2. Corner and centre: x0 = lon[0] - dx/2", fontsize=10)
    return fig


# ---------------------------------------------------------------- figure 3
def fig3():
    z, gt = checkerboard()
    src = mem(z, gt, "EPSG:4326")
    views = [
        ("Mollweide", canvas("+proj=moll", 0, 0, 3.62e7, 480, 240)),
        ("north polar Lambert azimuthal", canvas("+proj=laea +lat_0=90", 0, 0, 1.8e7)),
        ("EPSG:3031 polar stereographic", canvas("EPSG:3031", 0, 0, 1.2e7)),
        ("orthographic over Tasmania", canvas("+proj=ortho +lon_0=147 +lat_0=-43", 0, 0, 1.3e7)),
        ("Spilhaus", canvas("+proj=spilhaus", 0, 0, 2.2e7)),
    ]
    fig, ax = plt.subplots(2, 3, figsize=(13.5, 8.6), dpi=DPI)
    panel(ax.flat[0], z, "source: longitude/latitude, 720 x 360", cmap="viridis")
    for a, (title, spec) in zip(ax.flat[1:], views):
        w = gdal.Warp("", src, format="MEM", resampleAlg="bilinear", dstNodata=np.nan, **spec)
        panel(a, w.ReadAsArray(), title, cmap="viridis")
    fig.suptitle("Figure A3. One source, five canvases. Each canvas is only a CRS, an extent and a size; one gdal.Warp call fills it.", fontsize=11)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------- figure 4
def fig4():
    z, gt = checkerboard(step=0.1, wave=0.6)
    gdal.Translate("/vsimem/world.tif", mem(z, gt, "EPSG:4326"), creationOptions=["TILED=YES"])
    base = dict(format="MEM", **canvas("EPSG:3031", 0, 0, 1.2e7, 500, 500))
    small = 256 * 1024

    def w(**kw):
        return gdal.Warp("", "/vsimem/world.tif", **{**base, **kw}).ReadAsArray()

    cases = [
        ("near: -et 0.125 vs -et 0", w(resampleAlg="near"), w(resampleAlg="near", errorThreshold=0)),
        ("cubic, -et 0: -wm default vs 256 KB\n(kernel scale estimated per chunk)",
         w(resampleAlg="cubic", errorThreshold=0), w(resampleAlg="cubic", errorThreshold=0, warpMemoryLimit=small)),
        ("cubic, XSCALE=YSCALE=1: -wm default vs 256 KB\n(approximate transformer per chunk)",
         w(resampleAlg="cubic", warpOptions=["XSCALE=1", "YSCALE=1"]),
         w(resampleAlg="cubic", warpOptions=["XSCALE=1", "YSCALE=1"], warpMemoryLimit=small)),
        ("cubic, XSCALE=YSCALE=1 and -et 0:\n-wm default vs 256 KB",
         w(resampleAlg="cubic", errorThreshold=0, warpOptions=["XSCALE=1", "YSCALE=1"]),
         w(resampleAlg="cubic", errorThreshold=0, warpOptions=["XSCALE=1", "YSCALE=1"], warpMemoryLimit=small)),
    ]
    fig, ax = plt.subplots(1, 4, figsize=(15, 4.4), dpi=DPI)
    for a, (title, p, q) in zip(ax, cases):
        d = np.abs(p - q)
        n = int((d > 0).sum())
        a.imshow(p, cmap="gray", interpolation="nearest", alpha=0.35)
        a.imshow(np.ma.masked_equal(d, 0), cmap="magma_r", interpolation="nearest", vmin=-0.6 * max(d.max(), 1e-9), vmax=max(d.max(), 1e-9))
        a.set_title(f"{title}\n{n:,} of {d.size:,} pixels differ", fontsize=9)
        a.set_xticks([])
        a.set_yticks([])
    fig.suptitle("Figure A4. Where a warp's defaults change the answer: a 0.1-degree world to EPSG:3031, differences in colour over the result in grey.", fontsize=10, y=1.02)
    return fig


# ---------------------------------------------------------------- figure 5
def fig5():
    from osgeo import osr
    d = f"{FIX}/curvilinear"
    ll = f"{d}/swath-120x160-lonlat.tif"
    lon, lat = gdal.Open(ll).ReadAsArray()
    ny, nx = lon.shape
    jj, ii = np.mgrid[0:ny, 0:nx]
    blocks = ((jj // 4 + ii // 4) % 2).astype("float32")   # 4 x 4 source cells per block
    gdal.GetDriverByName("GTiff").CreateCopy("/vsimem/blocks.tif", mem(blocks, (0, 1, 0, 0, 0, 1), ""))
    wgs = osr.SpatialReference()
    wgs.ImportFromEPSG(4326)
    ext = (141, 163, -51.5, -36)
    w, h = 880, 620

    def warp(convention):
        v = gdal.Translate("", "/vsimem/blocks.tif", format="VRT")
        md = {"SRS": wgs.ExportToWkt(), "X_DATASET": ll, "X_BAND": "1", "Y_DATASET": ll, "Y_BAND": "2",
              "PIXEL_OFFSET": "0", "PIXEL_STEP": "1", "LINE_OFFSET": "0", "LINE_STEP": "1"}
        if convention:
            md["GEOREFERENCING_CONVENTION"] = convention
        v.SetMetadata(md, "GEOLOCATION")
        return gdal.Warp("", v, format="MEM", dstSRS="EPSG:4326", outputBounds=(ext[0], ext[2], ext[1], ext[3]),
                         width=w, height=h, resampleAlg="near", errorThreshold=0,
                         dstNodata=np.nan, geoloc=True).ReadAsArray()

    a, b = warp("PIXEL_CENTER"), warp(None)
    diff = (np.isfinite(a) != np.isfinite(b)) | (np.isfinite(a) & np.isfinite(b) & (a != b))
    cmap = ListedColormap(["#2b6c8f", "#f0c419"])

    def outline(x, col):
        x.plot(lon[0], lat[0], col, lw=0.8); x.plot(lon[-1], lat[-1], col, lw=0.8)
        x.plot(lon[:, 0], lat[:, 0], col, lw=0.8); x.plot(lon[:, -1], lat[:, -1], col, lw=0.8)

    fig, ax = plt.subplots(1, 3, figsize=(15, 4.6), dpi=DPI)
    for x, (arr, t) in zip(ax[:2], [(a, "GEOREFERENCING_CONVENTION=PIXEL_CENTER"),
                                     (b, "no convention line: TOP_LEFT_CORNER")]):
        x.imshow(arr, extent=ext, cmap=cmap, interpolation="nearest")
        outline(x, "k-")
        x.set_title(t, fontsize=9)
    zoom = (147.0, 149.5, -47.0, -45.0)
    ax[2].imshow(np.where(np.isfinite(a), a, np.nan), extent=ext, cmap=cmap, interpolation="nearest", alpha=0.55)
    ax[2].contour(np.flipud(np.nan_to_num(b, nan=-1)), levels=[0.5], extent=ext, colors="crimson", linewidths=1.2)
    ax[2].set_xlim(zoom[0], zoom[1]); ax[2].set_ylim(zoom[2], zoom[3])
    ax[2].set_title(f"zoom: PIXEL_CENTER blocks, TOP_LEFT_CORNER block edges in red\n{int(diff.sum()):,} of {int(np.isfinite(a).sum()):,} filled cells differ", fontsize=9)
    fig.suptitle("Figure A5. One swath (wxylib/fixtures curvilinear, 120 x 160, centre coordinates) in 4 x 4-cell blocks, read under both conventions; black lines are the outermost centres.", fontsize=10, y=1.02)
    return fig


# ---------------------------------------------------------------- figure 6
def fig6():
    z, gt = checkerboard(wave=0.0)
    src = mem(z.astype("uint8"), gt, "EPSG:4326", gdal.GDT_Byte)
    spec = canvas("+proj=ortho +lon_0=147 +lat_0=-43", 0, 0, 1.4e7)
    a = gdal.Warp("", src, format="MEM", resampleAlg="near", **spec)
    b = gdal.Warp("", src, format="MEM", resampleAlg="near", dstNodata=255, **spec)
    cmap = ListedColormap(["black", "white"])
    fig, ax = plt.subplots(1, 2, figsize=(9, 4.6), dpi=DPI)
    panel(ax[0], a.ReadAsArray(), f"no -dstnodata: nodata = {a.GetRasterBand(1).GetNoDataValue()}\noff the globe reads as 0, a valid value", cmap=cmap, vmin=0, vmax=1)
    bb = b.ReadAsArray()
    ax[1].set_facecolor("0.8")
    panel(ax[1], np.ma.masked_equal(bb, 255), "-dstnodata 255: empty cells are declared\n(masked, grey)", cmap=cmap, vmin=0, vmax=1)
    fig.suptitle("Figure A6. A 0/1 checkerboard (Byte) warped to orthographic. Without -dstnodata, empty and zero are the same number.", fontsize=10, y=1.02)
    return fig


if __name__ == "__main__":
    import matplotlib
    matplotlib.use("Agg")
    for i, f in enumerate((fig1, fig2, fig3, fig4, fig5, fig6), start=1):
        f().savefig(f"fig{i}.png", bbox_inches="tight", facecolor="white")
        print(f.__name__, "done")
