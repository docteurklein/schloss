with recursive cause (message_id, traversed, level) as (
    select m.message_id, '{}'::uuid[], 0
    from schloss.messages m
    where m.caused_by is null
    union all
    select consequence.message_id, traversed || consequence.caused_by, level + 1
    from cause
    join schloss.messages consequence on consequence.caused_by = cause.message_id
    where not consequence.message_id = any(traversed)
)
, leaf_consequence (caused_by, graph) as (
    select m.caused_by,
    jsonb_agg(to_jsonb(cause) - 'traversed' - 'policy')
    from cause
    join schloss.messages m using(message_id)
    where level > 0 and not message_id = any(traversed)
    group by m.caused_by
    union all
    select m.caused_by,
    (to_jsonb(m) - 'traversed' - 'policy')
    || jsonb_build_object('caused', graph)
    from leaf_consequence
    join schloss.messages m on m.message_id = leaf_consequence.caused_by
)
select jsonb_agg(graph)
from leaf_consequence
where caused_by is null;
