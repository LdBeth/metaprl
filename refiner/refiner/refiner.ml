IFDEF TERMS_ds THEN
   module Refiner = Refiner_ds.Refiner
ELSE
   IFDEF TERMS_std THEN
      module Refiner = Refiner_std.Refiner
   ELSE
      IFDEF TERMS_both THEN
         module Refiner = Refiner_debug.MakeRefinerDebug(Refiner_std.Refiner)(Refiner_ds.Refiner)
      ENDIF
   ENDIF
ENDIF
