with recursive cause as (
    select m.message_id, m.name, '{}'::uuid[] as traversed, 0 as level
    from schloss.messages m
    where m.caused_by is null
    union all
    select consequence.message_id, consequence.name, traversed || consequence.caused_by, level + 1
    from cause
    join schloss.messages consequence on consequence.caused_by = cause.message_id
    where not consequence.message_id = any(traversed)
    -- and (
    --     consequence.message_id = :'message_id'
    --     or consequence.caused_by = :'message_id'
    -- )
)
, leaf_consequence as (
    select m.caused_by,
    -- jsonb_agg(to_jsonb(cause) - 'traversed') as graph
    jsonb_agg(jsonb_build_object('name', cause.name)) as graph
    from cause
    join schloss.messages m using(message_id)
    where level > 0 and not message_id = any(traversed)
    group by m.caused_by
    union all
    select m.caused_by,
    -- (to_jsonb(m) - 'traversed')
    jsonb_build_object('name', m.name)
    || jsonb_build_object('caused', graph) as graph
    from leaf_consequence
    join schloss.messages m on m.message_id = leaf_consequence.caused_by
)
select jsonb_agg(graph)
from leaf_consequence
where caused_by is null;
