IFDEF TERMS_ds THEN
   module Refiner = Refiner_ds.Refiner
ELSE
   IFDEF TERMS_std THEN
      module Refiner = Refiner_std.Refiner
   ENDIF
ENDIF
