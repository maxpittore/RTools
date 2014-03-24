
update reg_tess_05
set urban_ratio=0;

update reg_tess_05
set urban_ratio = q.urban_ratio from 
(
SELECT tes.ogc_fid as id,sum(st_area (st_intersection (tes.the_geom,ua.the_geom)))/st_area(tes.the_geom) as urban_ratio
FROM reg_tess_05 as tes, ne_10m_urban_areas as ua
WHERE ST_Intersects(tes.the_geom,ua.the_geom) group by id) as q where q.id=ogc_fid;

--alter table reg_tess_05 add column urban_ratio numeric 