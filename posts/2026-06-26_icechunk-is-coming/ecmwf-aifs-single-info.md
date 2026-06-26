```
Driver: Icechunk

Dimensions:
  Name (path)  Size      Type      Direction
  -----------  ----  ------------  ---------
  /init_time   3264
  /latitude     721  HORIZONTAL_Y
  /lead_time     61
  /longitude   1440  HORIZONTAL_X

Coordinates (indexing variables):
  Name (path)   Dimension    Type              Unit
  -----------  -----------  -------  ------------------------
  /init_time   (init_time)  Int64    seconds since 1970-01-01
  /latitude    (latitude)   Float64  degree_north
  /lead_time   (lead_time)  Float64  seconds
  /longitude   (longitude)  Float64  degree_east

Data variables:
                  Name (path)                   Type              Unit                    Shape             Chunk size
  -------------------------------------------  -------  ------------------------  ---------------------  -----------------

 (/init_time):
  /expected_forecast_length                    Float64  seconds                   [3264]                 [23360]
  /ingested_forecast_length                    Float64  seconds                   [3264]                 [23360]

 (/init_time, /lead_time):
  /valid_time                                  Int64    seconds since 1970-01-01  [3264, 61]             [23360, 61]

 (/init_time, /lead_time, /latitude, /longitude):
  /dew_point_temperature_2m                    Float32  degree_Celsius            [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /downward_long_wave_radiation_flux_surface   Float32  W m-2                     [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /downward_short_wave_radiation_flux_surface  Float32  W m-2                     [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /geopotential_height_500hpa                  Float32  m                         [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /geopotential_height_850hpa                  Float32  m                         [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /geopotential_height_925hpa                  Float32  m                         [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /precipitation_surface                       Float32  kg m-2 s-1                [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /pressure_reduced_to_mean_sea_level          Float32  Pa                        [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /pressure_surface                            Float32  Pa                        [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /temperature_2m                              Float32  degree_Celsius            [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /temperature_850hpa                          Float32  degree_Celsius            [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /temperature_925hpa                          Float32  degree_Celsius            [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /total_cloud_cover_atmosphere                Float32  percent                   [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /wind_u_100m                                 Float32  m s-1                     [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /wind_u_10m                                  Float32  m s-1                     [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /wind_v_100m                                 Float32  m s-1                     [3264, 61, 721, 1440]  [1, 61, 241, 240]
  /wind_v_10m                                  Float32  m s-1                     [3264, 61, 721, 1440]  [1, 61, 241, 240]

Scalar arrays:
  Name (path)   Type   Unit
  ------------  -----  ----
  /spatial_ref  Int64

Attributes:
         Name           Type                                      Value
  -------------------  ------  ----------------------------------------------------------------------------
  attribution          String  "ECMWF AIFS Single forecast data processed by dynamical.org from ECMWF Open
                               Data."
  dataset_id           String  "ecmwf-aifs-single-forecast"
  dataset_version      String  "0.1.0"
  description          String  "Weather forecasts from the ECMWF Artificial Intelligence Forecasting System
                               (AIFS) Single model."
  forecast_domain      String  "Forecast lead time 0-360 hours (0-15 days) ahead"
  forecast_resolution  String  "6 hourly"
  license              String  "CC-BY-4.0"
  name                 String  "ECMWF AIFS Single forecast"
  spatial_domain       String  "Global"
  spatial_resolution   String  "0.25 degrees (~20km)"
  time_domain          String  "Forecasts initialized 2024-04-01 00:00:00 UTC to Present"
  time_resolution      String  "Forecasts initialized every 6 hours"

Arrays:

  - /init_time:
      Dimensions:    (/init_time)
      Shape:         [3264]
      Chunk size:    [23360]
      Type:          Int64
      Unit:          seconds since 1970-01-01
      Nodata value:  0

      Attributes:
                 Name            Type                       Value
        ----------------------  ------  ---------------------------------------------
        calendar                String  "proleptic_gregorian"
        long_name               String  "Forecast initialization time"
        standard_name           String  "forecast_reference_time"
        statistics_approximate  String  {"min":"2024-04-01T00:00:00","max":"Present"}

      Structural metadata:
        COMPRESSOR  { "name": "blosc", "configuration": { "typesize": 8, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } }

  - /latitude:
      Dimensions:    (/latitude)
      Shape:         [721]
      Chunk size:    [721]
      Type:          Float64
      Unit:          degree_north
      Nodata value:  "NaN"

      Attributes:
                 Name            Type          Value
        ----------------------  ------  --------------------
        _FillValue              String  "AAAAAAAA+H8="
        axis                    String  "Y"
        long_name               String  "Latitude"
        statistics_approximate  String  {"min":-90,"max":90}

      Structural metadata:
        COMPRESSOR  { "name": "blosc", "configuration": { "typesize": 8, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } }

  - /lead_time:
      Dimensions:    (/lead_time)
      Shape:         [61]
      Chunk size:    [61]
      Type:          Float64
      Unit:          seconds
      Nodata value:  "NaN"

      Attributes:
                 Name            Type                         Value
        ----------------------  ------  --------------------------------------------------
        _FillValue              String  "AAAAAAAA+H8="
        dtype                   String  "timedelta64[us]"
        long_name               String  "Forecast lead time"
        standard_name           String  "forecast_period"
        statistics_approximate  String  {"min":"0 days 00:00:00","max":"15 days 00:00:00"}

      Structural metadata:
        COMPRESSOR  { "name": "blosc", "configuration": { "typesize": 8, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } }

  - /longitude:
      Dimensions:    (/longitude)
      Shape:         [1440]
      Chunk size:    [1440]
      Type:          Float64
      Unit:          degree_east
      Nodata value:  "NaN"

      Attributes:
                 Name            Type             Value
        ----------------------  ------  -------------------------
        _FillValue              String  "AAAAAAAA+H8="
        axis                    String  "X"
        long_name               String  "Longitude"
        statistics_approximate  String  {"min":-180,"max":179.75}

      Structural metadata:
        COMPRESSOR  { "name": "blosc", "configuration": { "typesize": 8, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } }

  - /dew_point_temperature_2m:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          degree_Celsius
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "2 metre dewpoint temperature"
        short_name     String  "2d"
        standard_name  String  "dew_point_temperature"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /downward_long_wave_radiation_flux_surface:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          W m-2
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Surface downward long-wave radiation flux"
        short_name     String  "sdlwrf"
        standard_name  String  "surface_downwelling_longwave_flux_in_air"
        step_type      String  "avg"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /downward_short_wave_radiation_flux_surface:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          W m-2
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Surface downward short-wave radiation flux"
        short_name     String  "sdswrf"
        standard_name  String  "surface_downwelling_shortwave_flux_in_air"
        step_type      String  "avg"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /expected_forecast_length:
      Dimensions:    (/init_time)
      Shape:         [3264]
      Chunk size:    [23360]
      Type:          Float64
      Unit:          seconds
      Nodata value:  "NaN"

      Attributes:
                 Name            Type                         Value
        ----------------------  ------  --------------------------------------------------
        _FillValue              String  "AAAAAAAA+H8="
        dtype                   String  "timedelta64[us]"
        long_name               String  "Expected forecast length"
        statistics_approximate  String  {"min":"0 days 00:00:00","max":"15 days 00:00:00"}

      Structural metadata:
        COMPRESSOR  { "name": "blosc", "configuration": { "typesize": 8, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } }

  - /geopotential_height_500hpa:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          m
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Geopotential height"
        short_name     String  "gh"
        standard_name  String  "geopotential_height"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /geopotential_height_850hpa:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          m
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Geopotential height"
        short_name     String  "gh"
        standard_name  String  "geopotential_height"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /geopotential_height_925hpa:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          m
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Geopotential height"
        short_name     String  "gh"
        standard_name  String  "geopotential_height"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /ingested_forecast_length:
      Dimensions:    (/init_time)
      Shape:         [3264]
      Chunk size:    [23360]
      Type:          Float64
      Unit:          seconds
      Nodata value:  "NaN"

      Attributes:
                 Name            Type                         Value
        ----------------------  ------  --------------------------------------------------
        _FillValue              String  "AAAAAAAA+H8="
        dtype                   String  "timedelta64[us]"
        long_name               String  "Ingested forecast length"
        statistics_approximate  String  {"min":"0 days 00:00:00","max":"15 days 00:00:00"}

      Structural metadata:
        COMPRESSOR  { "name": "blosc", "configuration": { "typesize": 8, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } }

  - /precipitation_surface:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          kg m-2 s-1
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                       Value
        -------------  ------  ------------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        comment        String  "Average precipitation rate since the previous forecast step. Units equivalent
                               to mm/s."
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Precipitation rate"
        short_name     String  "prate"
        standard_name  String  "precipitation_flux"
        step_type      String  "avg"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /pressure_reduced_to_mean_sea_level:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          Pa
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Pressure reduced to MSL"
        short_name     String  "prmsl"
        standard_name  String  "air_pressure_at_mean_sea_level"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /pressure_surface:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          Pa
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Surface pressure"
        short_name     String  "sp"
        standard_name  String  "surface_air_pressure"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /spatial_ref:
      Dimensions:    ()
      Shape:         []
      Type:          Int64
      Nodata value:  0

      Attributes:
                   Name               Type                                        Value
        ---------------------------  -------  -----------------------------------------------------------------------------
        comment                      String   "This coordinate reference system matches the source data which follows WMO
                                              conventions of assuming the earth is a perfect sphere with a radius of 6,371,
                                              229m. It is similar to EPSG:4326, but EPSG:4326 uses a more accurate
                                              representation of the earth's shape."
        crs_wkt                      String   "GEOGCS[\"unknown\",DATUM[\"unknown\",SPHEROID[\"unknown\",6371229,0]],
                                              PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",
                                              0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AXIS[\"Longitude\",EAST],
                                              AXIS[\"Latitude\",NORTH]]"
        geographic_crs_name          String   "unknown"
        grid_mapping_name            String   "latitude_longitude"
        horizontal_datum_name        String   "unknown"
        inverse_flattening           Float64  0
        longitude_of_prime_meridian  Float64  0
        prime_meridian_name          String   "Greenwich"
        reference_ellipsoid_name     String   "unknown"
        semi_major_axis              Float64  6371229
        semi_minor_axis              Float64  6371229
        spatial_ref                  String   "GEOGCS[\"unknown\",DATUM[\"unknown\",SPHEROID[\"unknown\",6371229,0]],
                                              PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",
                                              0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AXIS[\"Longitude\",EAST],
                                              AXIS[\"Latitude\",NORTH]]"

      Structural metadata:
        COMPRESSOR  { "name": "zstd", "configuration": { "level": 0, "checksum": false } }

  - /temperature_2m:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          degree_Celsius
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "2 metre temperature"
        short_name     String  "2t"
        standard_name  String  "air_temperature"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /temperature_850hpa:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          degree_Celsius
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Temperature"
        short_name     String  "t"
        standard_name  String  "air_temperature"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /temperature_925hpa:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          degree_Celsius
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Temperature"
        short_name     String  "t"
        standard_name  String  "air_temperature"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /total_cloud_cover_atmosphere:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          percent
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "Total cloud cover"
        short_name     String  "tcc"
        standard_name  String  "cloud_area_fraction"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /valid_time:
      Dimensions:    (/init_time, /lead_time)
      Shape:         [3264, 61]
      Chunk size:    [23360, 61]
      Type:          Int64
      Unit:          seconds since 1970-01-01
      Nodata value:  0

      Attributes:
                 Name            Type                       Value
        ----------------------  ------  ---------------------------------------------
        calendar                String  "proleptic_gregorian"
        long_name               String  "Valid time"
        standard_name           String  "time"
        statistics_approximate  String  {"min":"2024-04-01T00:00:00","max":"Present"}

      Structural metadata:
        COMPRESSOR  { "name": "blosc", "configuration": { "typesize": 8, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } }

  - /wind_u_100m:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          m s-1
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "100 metre U wind component"
        short_name     String  "100u"
        standard_name  String  "eastward_wind"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /wind_u_10m:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          m s-1
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "10 metre U wind component"
        short_name     String  "10u"
        standard_name  String  "eastward_wind"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /wind_v_100m:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          m s-1
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "100 metre V wind component"
        short_name     String  "100v"
        standard_name  String  "northward_wind"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }

  - /wind_v_10m:
      Dimensions:    (/init_time, /lead_time, /latitude, /longitude)
      Shape:         [3264, 61, 721, 1440]
      Chunk size:    [1, 61, 241, 240]
      Type:          Float32
      Unit:          m s-1
      Nodata value:  "NaN"

      Attributes:
            Name        Type                                     Value
        -------------  ------  --------------------------------------------------------------------------
        _FillValue     String  "AAAAAAAA+H8="
        coordinates    String  "expected_forecast_length ingested_forecast_length spatial_ref valid_time"
        long_name      String  "10 metre V wind component"
        short_name     String  "10v"
        standard_name  String  "northward_wind"
        step_type      String  "instant"

      Structural metadata:
        COMPRESSOR  { "name": "sharding_indexed", "configuration": { "chunk_shape": [ 1, 61, 241, 240 ], "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "blosc", "configuration": { "typesize": 4, "cname": "zstd", "clevel": 3, "shuffle": "shuffle", "blocksize": 0 } } ], "index_codecs": [ { "name": "bytes", "configuration": { "endian": "little" } }, { "name": "crc32c" } ], "index_location": "end" } }
```
