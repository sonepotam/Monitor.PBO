funcDef calcKey with nSource:C local rV, rV1, rV2, rV3

  rV1 := 0
  for i := 2 to 12 step 2 do rV1 += val( subStr( nSource, i, 1 ))
  rV1 %= 10
  rV1 *= 3
  rV1 %= 10

  rV2 := 0
  for i := 1 to 13 step 2 do rV2 += val( subStr( nSource, i, 1 ))
  rV2 += rV1
  rV2 %= 10
  rV  := 10 - rV2
  rV  %= 10

  if empty( rV )
    rV1 := 0
    for i := 1 to 13 step 2 do rV1 += val( subStr( nSource, i, 1 ))
    rV1 %= 10
    rV3 := rV1
    rV1 *= 3
    rV1 %= 10

    rV2 := 0
    for i := 2 to 12 step 2 do rV2 += val( subStr( nSource, i, 1 ))
    rV2 += rV3
    rV2 %= 10
    rV  := 13 - rV2
  endIf
return nStr( int( rV ))
