-- THIS SCRIPT IS AUTOMATICALLY GENERATED. DO NOT EDIT IT DIRECTLY.
 CREATE OR REPLACE TABLE epinephrine AS
SELECT
  stay_id,
  linkorderid,
  rate AS vaso_rate,
  amount AS vaso_amount,
  starttime,
  endtime
FROM inputevents
WHERE
  itemid = 221289
