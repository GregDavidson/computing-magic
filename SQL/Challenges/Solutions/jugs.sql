-- * Water Jugs Problem

-- ** Statement of Problem

-- *** Given:

-- a little jug which can hold 3 liters
-- a big jug which can hold 5 liters

-- *** Goal:

-- Achieve exactly 4 liters in the big jug
-- with the fewest number of actions,
-- starting with both jugs empty

-- *** Allowed actions:

-- Fill either jug to its full capacity
-- Empty either jug completely
-- Pour one jug into the other until either
-- - the jug being poured from is empty
-- - the jug being poured into is full

-- Optimal or Heuristic Search not required
-- - Brute-force Breadth-First would suffice,
-- - or other methods not more than a small
-- - constant worse in time and space resources.

-- ** Strategy

-- Design a representation for the state of the two jugs
-- Design a method for
-- - evolving the state via allowed actions
-- - avoiding cycles
-- - either breadth-first or bounded depth-search

-- ** Simple Direct Approach

-- *** Represent the state as a row

select 0 as little, 0 as big, 'starting state' as method;

-- *** Generate and search statespace

WITH RECURSIVE jugs(little, big, method, depth) AS (
  SELECT 0, 0, 'starting state', 0
UNION
  SELECT foo.* FROM jugs j, LATERAL (
    SELECT 0, j.big, 'empty little jug', j.depth+1
      WHERE j.little > 0
  UNION
    SELECT j.little, 0, 'empty big jug', j.depth+1
      WHERE j.big > 0
  UNION
    SELECT 3, j.big, 'fill little jug', j.depth+1
      WHERE j.little < 3
  UNION
    SELECT j.little, 5, 'fill big jug', j.depth+1
      WHERE j.big < 5
  UNION
    SELECT 0, j.big + j.little, 'empty little into big', j.depth+1
      WHERE j.little > 0 AND j.big + j.little <= 5
  UNION
    SELECT j.big + j.little, 0, 'empty big into little', j.depth+1
      WHERE j.big > 0 AND j.big + j.little <= 3
  UNION
    SELECT 3, j.big - (3 - j.little), 'fill little from big', j.depth+1
      WHERE j.little < 3 AND j.big >= 3 - j.little
  UNION
    SELECT j.little - (5 - j.big), 5, 'fill big from little', j.depth+1
      WHERE j.big < 5 AND j.little >= 5 - j.big
  ) AS foo
)
  SEARCH BREADTH FIRST BY depth SET ordercol
  CYCLE little, big SET is_cycle USING path
 SELECT depth, path FROM jugs WHERE big = 4 ORDER BY ordercol LIMIT 1;
-- SELECT * FROM jugs ORDER BY ordercol LIMIT 10;
-- SELECT little, big, method, path FROM jugs ORDER BY ordercol LIMIT 150;

-- *** Criticism:

-- Method column isn't needed
-- - it can be inferred if desired
-- Symmetry would allow at least halving of SELECTs
-- The inequalities allow the same state to be produced multiple ways

-- ** Using a helper function for symmetry

CREATE OR REPLACE FUNCTION
jug1_jug2(
  j1 integer, j1_max integer,
  j2 integer, j2_max integer
) RETURNS TABLE(j1_prime integer, j2_prime integer) AS $$
  SELECT 0, j2                  -- empty j1
    WHERE j1 > 0
UNION
  SELECT j1_max, j2             -- fill j1
    WHERE j1 < j1_max
UNION
  SELECT 0, j1 + j2             -- empty j1 into j2
    WHERE j1 > 0 AND j1 + j2 <= j1_max
UNION
  SELECT j1_max, j2 - (j1_max - j1) -- fill j1 from j2
    WHERE j1 < j1_max AND j2 >= j1_max - j1
$$ LANGUAGE sql;

WITH RECURSIVE jugs(little, big, depth) AS (
  SELECT 0, 0, 0
UNION
  SELECT foo.*, j.depth+1 FROM jugs j, LATERAL (
    SELECT little_big.* FROM jug1_jug2(j.little, 3, j.big, 5) little_big
  UNION
    SELECT big_little.* FROM jug1_jug2(j.big, 5, j.little, 3) big_little
  ) AS foo
)
  SEARCH BREADTH FIRST BY depth SET ordercol
  CYCLE little, big SET is_cycle USING path
 SELECT depth, path FROM jugs WHERE big = 4 ORDER BY ordercol LIMIT 1;
-- SELECT * FROM jugs ORDER BY ordercol LIMIT 10;
-- SELECT little, big, path FROM jugs ORDER BY ordercol LIMIT 200;


-- *** Criticisms & Questions:

-- The inequalities in the last two SELECTs of jug1_jug2
-- allow the same state to sometimes be produced two different
-- ways.

-- How can I show all rows up to a particular condition, i.e.
-- all of the rows up to and including the first goal row?

-- Why is this method finding a different solution, and at
-- depth 7 rather than 6?
