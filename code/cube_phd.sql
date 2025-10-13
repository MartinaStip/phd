select *
from st_fact_student s
left join st_dim_obor o on
    s.idk_obor = o.idk_obor
left join st_dim_time t on
    s.idk_time = t.idk_time
where s.stav = 'S'
    and s.vykazovan_student = 'A' 
    and o.vykazovan_obor = 'A' 
    and o.typ = 'Doktorský'
    and t.datum = TO_DATE('2025-05-31', 'YYYY-MM-DD')
    and idk_prijezd is null;