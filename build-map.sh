#!/bin/sh

rm map/subunits.json
rm map/countries.json
rm map/regions.json
ogr2ogr -f GeoJSON map/subunits.json -where "ADM0_A3 = 'FRA'" map/ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp
ogr2ogr -f GeoJSON map/countries.json -where "ADM0_A3 != 'FRA' and ADM0_A3 != 'RUS' and ADM0_A3 != 'USA'" map/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp
ogr2ogr -f GeoJSON map/regions.json -where "ADM0_A3 = 'RUS' or ADM0_A3 = 'USA'" map/ne_10m_admin_1_states_provinces_lakes/ne_10m_admin_1_states_provinces_lakes.shp
topojson -o map/world.json --id-property ADM_A3,SU_A3,adm1_code --simplify 1e-6 -- map/countries.json map/subunits.json map/regions.json
