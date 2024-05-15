;==== all global variables
globals[
  flag ; what's this?
  ;cnt ; what's this?
  doIFarm
  showFarm
  ;startFarm
  sqKM              ;=300
  scale
  staticZeroNar

  births
  deaths

  migratedCount

  searchSpeed       ; 0.5; in kilometers/hour for an individual
  searchRadius      ; 0.0175 in kilometers/hour
  catchCost         ; 6*60 in kcals/hour
  searchCost        ; 4*60 in kcals/hour
  energyRequirement  ; 2000 in kcals/day * 365 days

  rePopChance ;  = 0.10;		 *was 0.05, now checks with paper*
	minNumToMigrate ; = 100;	 *was 10, now checks with paper*
	minKilosToSpread ;  = 400;	 *was 1000, now checks with paper*
	
  preyEnergy ;13800;			       in kcals per prey caught
	preyCatchTime ;3.916667;		   in hours per prey caught
	preyGrowthRate ;0.7;			     in individuals/year
	preyMaxNARAtPopulation ;200;
	
  cerealEnergy ;3390    ; in kcals per kilo gathered
  cerealHarvestTime  ;2;		 in hours per kilo gathered
	cerealGrowthRate ;1;
 	cerealMaxNARAtPopulation ;400  ;
  cerealMaxKilos ;= 600;			 in kilo's/hectare of cereal
	cerealMinKilos ;= 0;			 in kilo's/hectare of cereal
  cerealMaxHectaresPerKM ;1     ;	 percent/kilometer
	cerealMinHectaresPerKM ;1     ;	 percent/kilometer
  ;cNAR
	maxFarmHectares           ; = 3000;
	maxCerealKilosPerHectare  ; = 600;
	hectaresPerHour           ; = 2;
	farmMaxNARAtPopulation    ; = 800;

;  cerealFarmTime
	
	reproductionChance        ; = 0.02;
	minBandSize               ; = 20;
	maxBandSize               ; = 40;
	minMeat                   ; = false;
	minMeatPercent            ; = 0.1;
	maxForageTime             ; = 14;

  totalMigrated             ;= -1;
]

breed[farms farm]
breed[preys prey]
breed[cereals cereal]
breed[bands band]

;=== prey agent and its variables
preys-own [
  maxVal
  habitat ; 1-lush, 2-medium, 3-desert
  pop
  energy
  handling-time
  maxPop
  NAR
  ;tLeft
  growth
]

;=== cereal agent and its variables
cereals-own [
  habitat ; 1-lush, 2-medium, 3-desert
  pop
  energy
  handling-time
  farmPop
  maxFarm
  maxPop
  NAR
  ;tLeft
  growth
]

;=== farm agent and its variables
farms-own[
  habitat ; 1-lush, 2-medium, 3-desert
  maxFarm ; = (0.25 * (1000 - cerealGradient * p.maxCerealKilosPerHectare)
				  ;    + (cerealGradient * p.maxCerealKilosPerHectare)) * p.maxFarmHectares;  *was 600, changed to 1000, as paper records*
	cerealGradient;
  cerealKilos ; = (cerealGradient * (p.cerealMaxKilos - p.cerealMinKilos)) + p.cerealMinKilos;
	extraCerealKilos ;= 0.25 * (1000 - cerealKilos);  *was 600, changed to 1000, as paper records*
	cerealFarmTime ;= (1 / (p.hectaresPerHour * (cerealKilos + extraCerealKilos))) * 365 + p.cerealHarvestTime;
	maxFarmersDensity; = ((max * (p.cerealEnergy - cerealFarmTime * 6 * 60)) / (p.energyRequirement * 365)) / myCell.sqKM;
	NAR
  farmPop
  ;tLeft
  technology ; efficiency in which agriculture is practiced. More experienced lead to lower loss of soil
]

;=== band agent and its variables
bands-own [
  individuals
  habitat
  energyHunted
  energyGathered
  energyFarmed
  timeHunted
  timeGathered
  timeFarmed
  duration
]

; ===== setting values to all global variables
to set-parameters
  set flag false
  set doIFarm false
  set showFarm false
 ; set startFarm false
  ;set cnt 0
  set migratedCount 0
  set sqKM 300
  set scale 2
  set searchSpeed  0.5     ; in kilometers/hour
  set searchRadius 0.0175  ; in kilometers/hour
  set catchCost  (6 * 60)    ;in kcals/hour
  set searchCost (4 * 60)    ; in kcals/hour
  set energyRequirement 365 * 2000;  in kcals/year

  set rePopChance 0.10;		 *was 0.05, now checks with paper*
	set minNumToMigrate 100;	 *was 10, now checks with paper*
	set minKilosToSpread 400;	 *was 1000, now checks with paper*

  set maxForageTime 14;
  set maxFarmHectares sqKM * 0.1 * 100 ; 100 hectares per sqkm, 10% arable land per patch
  set maxCerealKilosPerHectare 600 ; does this need to be adjusted for sqkm too?
	set hectaresPerHour 2;
	set farmMaxNARAtPopulation 800;

  set cerealEnergy 3390    ; in kcals per kilo gathered
  set cerealHarvestTime  2;		 in hours per kilo gathered
	set cerealGrowthRate 1 ; one in paper, changed from 0.06;
	set cerealMaxNARAtPopulation 400  ;
  set cerealMaxKilos 600;			 in kilo's/hectare of cereal
	set cerealMinKilos 0;			 in kilo's/hectare of cereal
  set cerealMaxHectaresPerKM 1     ;	 percent/kilometer
	set cerealMinHectaresPerKM 1     ;	 percent/kilometer

  set preyEnergy 13800;			       in kcals per prey caught
	set preyCatchTime 3.916667;		   in hours per prey caught
	set preyGrowthRate 0.7 ; 0.7 in paper, changed from original value of 0.3;			     in individuals/year
	set preyMaxNARAtPopulation 200; what is this?

  set individualsAtStart 20

  set staticZeroNar -1000000 ; to avoid divide by zero errors when densities are 0 and searchtimes are infinite
end




;==== initialize the simulation
to setup
  clear-all

  set-parameters
  create-agents
  if not doIHunt? [set flag true]

  reset-ticks
end


to go
  procreate
  fission

  move

  growCerealsPreysFarms

  tick
end


to procreate
  set births 0
  ask bands [
    let band-births random-binomial individuals 0.02 ; binomial draws for birth counts
    set individuals individuals + band-births
    set births births + band-births
  ]
end

to fission
  ask bands [
  if individuals > (individualsAtStart * 2) [
    set individuals individuals - individualsAtStart
    ;let hab habitat
    ; adding a new band
    hatch-bands 1 [
      ;set SIZE 0.65
      ;set color black
      ;set shape "circle"
      ;      set habitat hab
     set individuals individualsAtStart

     set label individuals
     set label-color green
     ;if showFarm [ set individualsAtStart (individualsAtStart + 20) show individualsAtStart ]
   ;  ifelse doIHunt?  [
    ;    move-to max-one-of preys with [habitat = hab][preyNAR] ; when is preyNAR calculate?
    ;     ][
    ;    ifelse not showFarm [
    ;     move-to max-one-of cereals with [habitat = hab][cerealNAR]
    ;         ][ move-to max-one-of farms with [habitat = hab][NAR] ]
    ;       ]
      ]
    ]
  ]
end


to-report random-binomial [n p]
  report sum n-values n [ifelse-value (p > random-float 1) [1] [0]]
end

to move
  ask bands [
    decide
    let indvs 0
    ask bands-on farms-here [
      set indvs indvs + individuals
    ]
    set label indvs

    move-update

    set deaths 0
    eat-and-die


    let hab 0
    ask farms-here [ set hab habitat]
    ifelse hab != habitat [
      set migratedCount migratedCount + 1
      set habitat hab
      set duration 1
    ][set duration duration + 1]

 ]
end

to decide
  let bestCell one-of preys with [NAR > 0 and not any? other bands-here]
  ;----- go to prey first
  if doIHunt? [
    set flag false
    set bestCell one-of preys with [NAR > 0 and not any? other bands-here]
    ;if cnt = 0 [ show "go for prey" set cnt 1]
    ask preys [
      set NAR calculateNAR  ; this should be update more often? because it changes with density
      if bestCell != nobody and NAR > (max list 0 [NAR] of bestCell) [
        set bestCell one-of preys-here
      ]
      if bestCell = nobody [
        set flag true
      ]
    ]
  ]

  ;----- then go for cereals
  if flag [
    set bestCell one-of cereals with [NAR > 0 and not any? other bands-here]
   ; if cnt <= 1 [ show "go for cereals" set cnt 2]
    ask cereals [
      set NAR calculateNAR
      if bestCell != nobody and NAR > (max list 0 [NAR] of bestCell) [
        set bestCell one-of cereals-here
      ]
    ]

  ]
  ifelse bestCell != nobody [
    move-to one-of farms-on bestCell
   ; let cAvg averageCereal
   ; if cAvg < 185 [ ; why is this necessary?
    ;   set startFarm true
       ;show "startFarm"
    ;]
  ]
  [
    ;---finally move to farm
    if doFarm?  [ ; and doIFarm and startFARM -- what does that do?
      ;----go for farm
      let cAvg averageCereal
      ; FarmAverage 100 showing the farm
    ;  if cAvg < 50 [set showFarm true ]
        set bestCell one-of farms-here
        ask farms [
          if NAR > [NAR] of bestCell and canSustainMoreFarmers [
            set bestCell one-of farms-here
          ]
        ]

;        ask farms-here [
;          if (not hasFreeLand) [
;            ask farms[
;              if canSustainMoreFarmers[
;                set bestCell one-of farms-here
;                ; show (word " not hasFreeLand bestcell is " bestCell)
;              ]
;            ]
;          ]
;        ]
        move-to bestCell
      ]
  ]
end

to-report averageCereal
  report mean [pop] of cereals / sqKM ; sqrKm
end

to growCerealsPreysFarms
   ;==== growing cereals OR Farm
   ask cereals [
    if maxPop > 0 [
     ifelse (pop <= 0) [
      let cereal-neighbors other cereals-on neighbors
      let viable-neighbors count cereal-neighbors with [pop >= minKilosToSpread]
      if viable-neighbors > 0 [ ; this could/should happen once for every viable neighbor?
        if random-float 1 < rePopChance[
         	set pop minKilosToSpread
        ]
     ]
    ][
        set pop pop * (maxPop * growth) / (maxPop - (pop * (1 - growth))) ; this is correct, but note the equation is wrong in the paper
     ]
            set nar calculateNAR
    update-farms
  ]
  ]


  ; growing preys
  ask preys with [doIHunt?] [
        if maxPop > 0 [
    ifelse (pop <= 0) [ ; with all this wrapped in ask preys here, it means you conceivably have a restored patch restore another patch in the same time step -- should separate out
      let prey-neighbors other preys-on neighbors
      let viable-neighbors count prey-neighbors with [pop >= minNumToMigrate]
      if viable-neighbors > 0 [ ; this could/should happen once for every viable neighbor?
        if random-float 1 < rePopChance[
         	set pop minNumToMigrate
        ]
     ]
    ][
			set pop pop * (maxPop * growth) / (maxPop - (pop * (1 - growth)))  ; this is correct, but note the equation is wrong in the paper
  	]
            set nar calculateNAR
    update-preys
  ]
  ]
end

;to-report energyRequired [totalEnergy primaryNAR otherNAR]
;		let otherTotal  0;
;		set primaryNAR (max list 0 primaryNAR)
;    foreach otherNAR [ x -> (set otherTotal otherTotal + (max list 0 x)) ]
;		report (totalEnergy * primaryNAR / (primaryNAR + otherTotal))
;end

to-report diet-proportion [pNAR cNAR fNAR]
  set pNAR (max list 0 pNAR)
   set cNAR (max list 0 cNAR)
   set fNAR (max list 0 fNAR)
  if fNAR < cNAR [ set fNAR 0 ] ; only farm if NAR is greater than that for gathering
  let total-nar pNAr + cNAR + fNAR
  if total-nar = 0 [ report (list 0 0 0) ]
  let prey-percent pNAR / total-nar
  let cereal-percent cNAR / total-nar
  let farm-percent fNAR / total-nar
  report (list prey-percent cereal-percent farm-percent)
end



; need to update nar at some point

to-report calculateEnergyReturns [ energyNeeded timeAvailable agent ]
  let agentNAR [ nar ] of agent
  let agentPop [ pop ] of agent
  let gather-time [ handling-time ] of agent + calc-search-time (agentPop / sqKm)

  ; this version doesn't lose energy explicitly from search and capture, but its implicit in the use of NAR, right?

  let gatherable (min list agentPop (timeAvailable / gather-time))
  let energy-per-unit agentNAR * gather-time
  let units-needed energyNeeded / energy-per-unit
  let units-gathered min list gatherable units-needed
  let energy-obtained units-gathered * energy-per-unit
  let time-used units-gathered * gather-time

  report (list energy-obtained time-used units-gathered)
end

to-report calculateFarmEnergy [energyNeeded timeAvailable farmAgent]
  		let fNAR [nar] of farmAgent
  let farm-time [cerealFarmTime] of farmAgent
  ;let tech [technology] of farmAgent
    let cerealFarmable (timeAvailable / farm-time)

    let timeToFarm (energyNeeded / fNAR)
		let cerealNeeded energyNeeded / cerealEnergy

		
  let cerealFarmed (min list cerealFarmable cerealNeeded)
	set cerealFarmed (max list 0 cerealFarmed);


  let totduration 0
  ask bands-here [ set totduration totduration + duration]
;  ifelse totduration = 0
 ;  [set technology 1 / (1 + learningfactor)]
 ;  [set technology (totduration / (totduration + learningfactor))]

  let output [0 0]
  set output replace-item 0 output (cerealFarmed * cerealEnergy)
	set output replace-item 1 output (cerealFarmed * farm-time)

 ; set farmPop (min list maxFarm (farmPop * technology + cerealFarmed )) ; is this actually necessary?

  ;if (cerealFarmed * cerealFarmTime) = 0 [show(word "farm population: " farmPop)]
		;check-nan farmPop
    ;show(word "calculateFarmEnergy cerealFarmTime: " cerealFarmTime)
    ;show(word "calculateFarmEnergy cerealFarmed: " cerealFarmed)
	  ;show(word "farm output: " output)
    report output

end

to eat-and-die


; Retrieve agents present at the band's location.
  let preyAgent one-of preys-here
  let cerealAgent one-of cereals-here
  let farmAgent one-of farms-here

  ; Calculate total energy requirement and total time available for foraging.
		let energyLeft individuals * energyRequirement ; total annual energy requirement for band
		let timeLeft 365 * individuals * maxForageTime ; total foraging time for band
  	;let results [0 0] ; list to store results, first item is energy and second is time (?)

  ; Initialize energy variables to 0 for the current time step.
  set energyHunted 0
  set energyGathered 0
  set energyFarmed 0

  ; reset times to zero
		set timeHunted 0
    set timeGathered 0
    set timeFarmed 0

  ; get NARs for the current patch
  let pNAR ifelse-value doIHunt? [ [NAR] of preyAgent ] [ 0 ]
    let cNAR [NAR] of cerealAgent ; agents always gather cereals
  	let fNAR ifelse-value doFarm? [ [NAR] of farmAgent ] [ 0 ]

  ; decide whether its worth farming
  	;if doFarm? [set doIFarm  (round cNar) < (round fNar)] ; band decides to farm if cereal NAR less than farming NAR ; why is the rounding necessary?

   let diet diet-proportion pnar cnar fnar ; get a list of the proportion of diet for each food type, based on NAR

  ; determine energy required from prey
  let preyEnergyRequired energyLeft * item 0 diet ;(energyRequired energyLeft pNAR (list cNAR fNAR)) ; is this necessary anymore?
  let cerealEnergyRequired energyLeft * item 1 diet
  let farmEnergyRequired energyLeft * item 2 diet

  if (doIHunt? and preyEnergyRequired > 0 and pNAR > 0) [
    let results calculateEnergyReturns preyEnergyRequired timeLeft preyAgent
    set energyHunted item 0 results
    set timeHunted item 1 results
    ask preyAgent [
      set pop pop - item 2 results
      set nar calculateNAR
    ]  ; Adjusting population

     set energyLeft energyLeft - energyHunted
     set timeLeft timeLeft - timeHunted
  ]
  ; Determine energy required from cereals
  ;let cerealEnergyRequired (ifelse-value doIFarm [energyRequired energyLeft cNAR (list 0 fNAR)] [energyLeft])
  if (cerealEnergyRequired > 0 and cNAR > 0) [
       let results calculateEnergyReturns cerealEnergyRequired timeLeft cerealAgent

    set energyGathered item 0 results
    set timeGathered item 1 results
    ask cerealAgent [
      set pop pop - item 2 results
           set nar calculateNAR
    ]  ; Adjusting population
     set energyLeft energyLeft - energyGathered
     set timeLeft timeLeft - timeGathered
  ]

  ; Determine energy required from farming
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; does farming depnd on wild cereal availability or not? I think not . . .
  if (doFarm? and farmEnergyRequired > 0 and fNAR > 0) [ ; are all these checks necessary or are some redundant? ;what's startfarm again?
    let results calculateFarmEnergy farmEnergyRequired timeLeft farmAgent

    set energyFarmed item 0 results
    set timeFarmed item 1 results
    ask farmAgent [
     ;set pop pop - item 2 results ; not a thing here?
           set nar calculateFarmNAR
    ]
     set energyLeft energyLeft - energyFarmed
     set timeLeft timeLeft - timeFarmed
  ]



  if energyLeft > .1 [
    let band-deaths ceiling (energyLeft / energyRequirement)
    set band-deaths min list band-deaths individuals
    set individuals individuals - band-deaths
    set deaths deaths + band-deaths
    if individuals <= 0 [ die ]

  ]

end


to-report calc-search-time [ density ]
  if density = 0 [ report staticZeroNar * -1 ] ; should change variable name and remove -1
  report 1 / (searchSpeed * searchRadius * 2 * density)
end

to-report calculateNAR
  let search-time calc-search-time (pop / sqKm)
  report (energy - handling-time * catchCost - search-time * searchCost) / (handling-time + search-time)
end


to-report calculateFarmNAR
  report cerealEnergy / cerealFarmTime - catchCost ; is this the same value for catchCost as others or should it be a different parameter?
end



to check-nan [value]
  if not is-number? value [
    print (word value " is NaN")
  ]
end


to-report calcMaxFarm
  ;show (word "calcMaxFarm habitat 2 cerealKilos is: " cerealGradient)
   let maxF ((0.25 * (1000 - cerealGradient * maxCerealKilosPerHectare) + (cerealGradient * maxCerealKilosPerHectare)) * maxFarmHectares) ; is this correct?
   set maxF (maxF ^ degradationFactor)
   report maxF
end

to-report hasFreeLand
  let indvs 0
  if bands-here != nobody [set indvs sum [individuals] of bands-here]
  ;show(word "individuals: " indvs)
  let energyNeeded (indvs * energyRequirement);
  let timeToFarm  (energyNeeded / NAR);
	let cerealNeeded ((energyNeeded + timeToFarm * 6 * 60) / cerealEnergy);		
  ;show(word "cerealNeeded: " cerealNeeded)
  ;show(word "maxFarm - farmPop: " (maxFarm - farmPop))
  report (maxFarm - farmPop) >= cerealNeeded
end

to-report canSustainMoreFarmers
  let indvs sum [individuals] of bands-here
  ;if bands-here != nobody [set indvs [individuals] of one-of bands-here]
  let getIndDensity indvs / (sqKM / 20) ;;;; this shouldn't be hardcoded
   ;show(word "indvs: " indvs)
  ;show(word "curDensity: " getIndDensity)
  ;show(word "maxFarmersDensity: " maxFarmersDensity)
  if (getIndDensity > maxFarmersDensity) [
    ;show count bands-here
    ;show(word "indvs: " indvs)
    ;show(word "curDensity: " getIndDensity)
    ;show(word "maxFarmersDensity: " maxFarmersDensity)
    ;let indvs mean [individuals] of bands-here
    ;let curDensity (getIndDensity + indvs / sqKM)
  ]
	report  getIndDensity < maxFarmersDensity;
end



; ==== generate all agents based on each habitat
to create-agents
  ;============= lush = 1 habitat
  ask patches with [pxcor mod 1 = 0 and pycor mod 1 = 0 and pxcor <= -4]
  [
    set pcolor 127
    sprout-farms 1  [
      set SIZE 1.15
      set color white
      set shape "square"
      set habitat 1  ; lush
      set farmPop 0
      set cerealGradient 1.0
      set cerealKilos ((cerealGradient * (cerealMaxKilos - cerealMinKilos)) + cerealMinKilos);
	    set extraCerealKilos (0.25 * (1000 - cerealKilos)) ;  *was 600, changed to 1000, as paper records*	
      set cerealFarmTime ((1 / (hectaresPerHour * (cerealKilos + extraCerealKilos))) * 365 + cerealHarvestTime);
      set maxFarm calcMaxFarm
      set NAR calculateFarmNar  ; function call
      set maxFarmersDensity (((maxFarm * (cerealEnergy - cerealFarmTime * 6 * 60)) / (energyRequirement)) / (sqKM)); * scale));
      ;show (word "Farm habitat 1 maxFarm is: " maxFarm )
      ;show (word "Farm habitat 1 StaticNAR is: " staticNAR )
      ;show (word "Farm habitat 1 cerealFarmTime is: " cerealFarmTime)
    ]
    sprout-preys 1  [
      set SIZE 0.96
      set shape "square1"
      set color 14 ; red
      set habitat 1  ; lush
      set growth e ^ preyGrowthRate  ; 0.7
      set maxPop 8 * sqKM
      set pop 8 * sqKM
      set energy preyEnergy
      set handling-time preyCatchTime
      set NAR calculateNAR
      ;show preyNar
      set xcor xcor - 0.2
    ]
    sprout-cereals 1  [
      set SIZE 0.96
      set color green - 3
      set shape "square1"
      set habitat 1  ; lush
      set growth e ^ cerealGrowthRate ; 1
      set maxPop 600 * sqKM
      set pop 600 * sqKM
      set energy cerealEnergy
      set handling-time cerealHarvestTime
      set NAR calculateNAR
      ;show cerealNar
      set xcor xcor + 0.2
    ]
    if (random-float 1 < 0.01) [
      new-bands 1
    ]
  ]

  ;=============setting medium habitat
  ask patches with [pxcor mod 1 = 0 and pycor mod 1 = 0 and pxcor > -4 and pxcor <= 3]
  [
    set pcolor 66
    sprout-farms 1  [
      set SIZE 1.15
      set color white
      set shape "square"
      set habitat 2  ; medium
      set farmPop 0
      set cerealGradient 0.67
      set cerealKilos ((cerealGradient * (cerealMaxKilos - cerealMinKilos)) + cerealMinKilos);
      set extraCerealKilos (0.25 * (1000 - cerealKilos)) ;  *was 600, changed to 1000, as paper records*	
      set cerealFarmTime ((1 / (hectaresPerHour * (cerealKilos + extraCerealKilos))) * 365 + cerealHarvestTime);
      set maxFarm calcMaxFarm
      set NAR calculateFarmNar  ; function call
      set maxFarmersDensity (((maxFarm * (cerealEnergy - cerealFarmTime * 6 * 60)) / (energyRequirement)) / (sqKM)) ;* scale));
      ;show (word "Farm habitat 2 maxFarm is: " maxFarm )
      ;show (word "Farm habitat 2 StaticNAR is: " staticNAR )
      ;show (word "Farm habitat 2 cerealFarmTime is: " cerealFarmTime)
    ]
    sprout-preys 1 [
      set shape "square4"
      set habitat 2  ; medium
      set color 14; red
      set growth e ^ preyGrowthRate  ; 0.7
      set maxPop 5.4 * sqKM
      set pop 5.4 * sqKM
      set energy preyEnergy
      set handling-time preyCatchTime
      set NAR calculateNAR
      ;show preyNar
      set xcor xcor - 0.2
    ]
    sprout-cereals 1 [
      set color green - 2
      set shape "square4"
      set habitat 2  ; medium
      set growth e ^ cerealGrowthRate ; 1
      set maxPop 400 * sqKM ; 402, why not 400 as in paper?
      set pop 400 * sqKM; 402, why not 400 as in paper?
      set energy cerealEnergy
      set handling-time cerealHarvestTime
      set NAR calculateNAR
      ;show cerealNar
      set xcor xcor + 0.2
    ]
    if (random-float 1 < 0.01) [
      new-bands 2
    ]
  ]
  ;=============setting desert habitat
  ask patches with [pxcor mod 1 = 0 and pycor mod 1 = 0 and pxcor > 3]
  [
    set pcolor 44 ;yellow
    sprout-farms 1  [
      set SIZE 1.15
      set color white
      set shape "square"
      set habitat 3  ; desert
      set farmPop 0
      set cerealGradient 0.33
      set cerealKilos ((cerealGradient * (cerealMaxKilos - cerealMinKilos)) + cerealMinKilos);
	    set extraCerealKilos (0.25 * (1000 - cerealKilos)) ;  *was 600, changed to 1000, as paper records*	
      set cerealFarmTime ((1 / (hectaresPerHour * (cerealKilos + extraCerealKilos))) * 365 + cerealHarvestTime);
      set maxFarm calcMaxFarm
      set NAR calculateFarmNar  ; function call
      set maxFarmersDensity (((maxFarm * (cerealEnergy - cerealFarmTime * 6 * 60)) / (energyRequirement)) / (sqKM)); * scale));
      ;show (word "Farm habitat 3 maxFarm is: " maxFarm )
      ;show (word "Farm habitat 3 StaticNAR is: " staticNAR )
      ;show (word "Farm habitat 3 cerealFarmTime is: " cerealFarmTime)
    ]
    sprout-preys 1 [
      set shape "square7"
      set color 14 ; red
      set habitat 3  ; desert
      set growth e ^ preyGrowthRate  ; 0.7
      set maxPop 2.6 * sqKM
      set pop 2.6 * sqKM
      set energy preyEnergy
      set energy preyEnergy
      set handling-time preyCatchTime
      set NAR calculateNAR
      ;show preyNar
      set xcor xcor - 0.2
    ]
    sprout-cereals 1  [
      set shape "square7"
      set color green - 1
      set habitat 3  ; desert

      set growth e ^ cerealGrowthRate ; 1
      set maxPop 200 * sqKM; originally 198, why not 200 as in paper?
      set pop 200 * sqKM; originally 198, why not 200 as in paper?
           set energy cerealEnergy
      set handling-time cerealHarvestTime
      set NAR calculateNAR
      ;show cerealNar
      set xcor xcor + 0.2
    ]
    if (random-float 1 < 0.01) [
      new-bands 3
    ]
    if count bands < 2 [
      new-bands 3
    ]
  ]
end

to new-bands [habit]
  sprout-bands 1 [
    set SIZE 0.4
    set color black
    set shape "circle"
    set habitat habit
    set individuals individualsAtStart
    set label individuals
    set label-color green
    set duration 1
  ]
end

to update-preys ; update prey visualization
  if habitat = 1 [
      if pop > (maxPop * 7 / 8 )  [
          set shape "square1"
      ] if pop >= (maxPop * 6 / 8 ) and pop < (maxPop * 7 / 8 ) [
           set shape "square2"
      ] if pop >= (maxPop * 5 / 8 ) and pop < (maxPop * 6 / 8 ) [
           set shape "square3"
      ] if pop >= (maxPop * 4 / 8 ) and pop < (maxPop * 5 / 8 ) [
           set shape "square4"
      ] if pop >=(maxPop * 3 / 8 ) and pop < (maxPop * 4 / 8 ) [
          set shape "square5"
      ] if pop >= (maxPop * 2 / 8 ) and pop < (maxPop * 3 / 8 ) [
          set shape "square6"
      ] if pop >= (maxPop * 1.5 / 8 ) and pop < (maxPop * 2 / 8 ) [
          set shape "square7"
      ] if pop >= (maxPop * 1 / 8 ) and pop < (maxPop * 1.5 / 8 ) [
          set shape "square8"
      ] if pop >= (maxPop * 0.7 / 8 ) and pop < (maxPop * 1 / 8 ) [
          set shape "square9"
      ] if pop < (maxPop * 0.7 / 8 ) [
        set shape "squareb"
      ]
    ]
    if habitat = 2 [
      if pop > (maxPop * 4 / 5 ) [
           set shape "square4"
      ] if pop >= (maxPop * 3 / 5 ) and pop < (maxPop * 4 / 5 ) [
          set shape "square5"
      ] if pop >= (maxPop * 2 / 5 ) and pop < (maxPop * 3 / 5 ) [
          set shape "square6"
      ] if pop >= (maxPop * 1 / 5 ) and pop < (maxPop * 2 / 5 ) [
          set shape "square7"
      ] if pop >= (maxPop * 1.5 / 5 ) and pop < (maxPop * 2 / 5 ) [
          set shape "square8"
      ] if pop >= (maxPop * 1 / 5 ) and pop < (maxPop * 1.5 / 5 ) [
          set shape "square9"
      ] if pop < (maxPop * 1 / 5 )  [
        set shape "squareb"
      ]
    ]
    if habitat = 3 [
      if pop > (maxPop * 2 / 3 ) [
           set shape "square7"
      ] if pop >= (maxPop * 1 / 3 ) and pop < (maxPop * 2 / 3 ) [
          set shape "square8"
      ] if pop >= (maxPop * 0.5 / 3 ) and pop < (maxPop * 1 / 3 ) [
          set shape "square9"
      ] if pop < (maxPop * 0.5 / 3 ) [
        set shape "squareb"
      ]
    ]
end
to update-farms ; update visualization of farms
       ifelse showFarm [
     ;=== growing farms
        set farmPop sum [farmPop] of farms-here
        ;show farmPop
        set maxFarm sum [maxFarm] of farms-here
        ;show maxFarm
        set color lime
        if habitat = 1 [
          if farmPop > (maxFarm * 9 / 10 )  [
            set shape "farm1"
          ] if farmPop >= (maxFarm * 8 / 10 ) and farmPop < (maxFarm * 9 / 10 ) [
            set shape "farm2"
          ] if farmPop >= (maxFarm * 7 / 10 ) and farmPop < (maxFarm * 8 / 10 ) [
            set shape "farm3"
          ] if farmPop >= (maxFarm * 6 / 10 ) and farmPop < (maxFarm * 7 / 10) [
            set shape "farm4"
          ] if farmPop >=(maxFarm * 5 / 10 ) and farmPop < (maxFarm * 6 / 10 ) [
            set shape "farm5"
          ] if farmPop >= (maxFarm * 4 / 10 ) and farmPop < (maxFarm * 5 / 10 ) [
            set shape "farm6"
          ] if farmPop >= (maxFarm * 3 / 10) and farmPop < (maxFarm * 4 / 10 ) [
            set shape "farm7"
          ]  if farmPop >= (maxFarm * 2 / 10) and farmPop < (maxFarm * 3 / 10 ) [
            set shape "farm8"
          ] if farmPop > (maxFarm * 1.5 / 10 ) and farmPop < (maxFarm * 2 / 10 ) [
            set shape "farm9"
          ] if farmPop >= (maxFarm * 1 / 10 ) and farmPop < (maxFarm * 1.5 / 10 ) [
            set shape "farm10"
          ] if farmPop < (maxFarm * 1 / 10 ) [
            set shape "squareb"
          ]
       ]
       if habitat = 2 [
          if farmPop > (maxFarm * 5 / 6 )  [
            set shape "farm5"
          ] if farmPop >= (maxFarm * 4 / 6 ) and farmPop < (maxFarm * 5 / 6 ) [
            set shape "farm6"
          ] if farmPop >= (maxFarm * 3 / 6) and farmPop < (maxFarm * 4 / 6 ) [
            set shape "farm7"
          ]  if farmPop >= (maxFarm * 2 / 6) and farmPop < (maxFarm * 3 / 6 ) [
            set shape "farm8"
          ] if farmPop > (maxFarm * 1.5 / 6 ) and farmPop < (maxFarm * 2 / 6 ) [
            set shape "farm9"
          ] if farmPop >= (maxFarm * 1 / 6 ) and farmPop < (maxFarm * 1.5 / 6 ) [
            set shape "farm10"
          ] if farmPop < (maxFarm * 1 / 6 ) [
            set shape "squareb"
          ]
       ]
       if habitat = 3 [
          if farmPop > (maxFarm * 2.3 / 3) [
            set shape "farm8"
          ] if farmPop > (maxFarm * 1.5 / 3 ) and farmPop < (maxFarm * 2.3 / 3 ) [
            set shape "farm9"
          ] if farmPop >= (maxFarm * 1 / 3 ) and farmPop < (maxFarm * 1.5 / 3 ) [
            set shape "farm10"
          ] if farmPop < (maxFarm * 1 / 3 ) [
            set shape "squareb"
          ]
       ]
    ][
      if habitat = 1 [
        if pop > (maxPop * 9 / 10 )  [
          set shape "square1"
        ] if pop >= (maxPop * 8 / 10 ) and pop < (maxPop * 9 / 10 ) [
          set shape "square1_"
        ] if pop >= (maxPop * 7 / 10 ) and pop < (maxPop * 8 / 10 ) [
          set shape "square2"
        ] if pop >= (maxPop * 6 / 10 ) and pop < (maxPop * 7 / 10) [
          set shape "square3"
        ] if pop >=(maxPop * 5 / 10 ) and pop < (maxPop * 6 / 10 ) [
          set shape "square4"
        ] if pop >= (maxPop * 4 / 10 ) and pop < (maxPop * 5 / 10 ) [
          set shape "square5"
        ] if pop >= (maxPop * 3 / 10) and pop < (maxPop * 4 / 10 ) [
          set shape "square6"
        ]  if pop >= (maxPop * 2 / 10) and pop < (maxPop * 3 / 10 ) [
          set shape "square7"
        ] if pop > (maxPop * 1 / 10 ) and pop < (maxPop * 2 / 10 ) [
          set shape "square8"
        ] if pop >= (maxPop * 0.5 / 10 ) and pop < (maxPop * 1 / 10 ) [
          set shape "square9"
        ] if pop < (maxPop * 0.5 / 3 ) [
          set shape "squareb"
        ]
      ]
      if habitat = 2 [
        if pop > (maxPop * 4 / 5 ) [
          set shape "square4"
        ] if pop >= (maxPop * 3 / 5 ) and pop < (maxPop * 4 / 5 ) [
          set shape "square5"
        ] if pop >= (maxPop * 2 / 5 ) and pop < (maxPop * 3 / 5 ) [
          set shape "square6"
        ] if pop >= (maxPop * 1.5 / 5 ) and pop < (maxPop * 2 / 5 ) [
          set shape "square7"
        ] if pop >= (maxPop * 1 / 5 ) and pop < (maxPop * 1.5 / 5 ) [
          set shape "square8"
        ] if pop >= (maxPop * 0.5 / 5 ) and pop < (maxPop * 1 / 5 ) [
          set shape "square9"
        ] if pop < (maxPop * 0.5 / 5 ) [
          set shape "squareb"
        ]
      ]
      if habitat = 3 [
        if pop > (maxPop * 2 / 3 ) [
          set shape "square7"
        ] if pop >= (maxPop * 1 / 3 ) and pop < (maxPop * 2 / 5 ) [
          set shape "square8"
        ] if pop >= (maxPop * 0.5 / 3 ) and pop < (maxPop * 1 / 3 ) [
          set shape "square9"
        ] if pop < (maxPop * 0.5 / 3 ) [
          set shape "squareb"
        ]
      ]
    ]
end

to move-update

    ask cereals-here  [
      ifelse not showFarm [
        if pop > (maxPop * 9 / 10 )  [
          set shape "square1_"
        ] if habitat = 1 and pop >= (maxPop * 8 / 10 ) and pop < (maxPop * 9 / 10 ) [
          set shape "square2"
        ] if habitat = 1 and pop >= (maxPop * 7 / 10 ) and pop < (maxPop * 8 / 10 ) [
          set shape "square3"
        ] if habitat <= 2 and pop >= (maxPop * 6 / 10 ) and pop < (maxPop * 7 / 10) [
          set shape "square4"
        ] if habitat <= 2 and pop >=(maxPop * 5 / 10 ) and pop < (maxPop * 6 / 10 ) [
          set shape "square5"
        ] if habitat <= 2 and pop >= (maxPop * 4 / 10 ) and pop < (maxPop * 5 / 10 ) [
          set shape "square6"
        ] if pop >= (maxPop * 3 / 10) and pop < (maxPop * 4 / 10 ) [
          set shape "square7"
        ]  if pop >= (maxPop * 2 / 10) and pop < (maxPop * 3 / 10 ) [
          set shape "square8"
        ] if pop >= (maxPop * 1 / 10) and  pop < (maxPop * 2 / 10 ) [
          set shape "square9"
        ] if pop < 100 [
          set shape "squareb"
        ]
     ]
     [
      set farmPop sum [farmPop] of farms-here
        ;show farmPop
      set maxFarm sum [maxFarm] of farms-here
        ;show maxFarm
      set color lime
      if farmPop > (maxFarm * 9 / 10 )  [
            set shape "farm1"
      ] if farmPop >= (maxFarm * 8 / 10 ) and farmPop < (maxFarm * 9 / 10 ) [
            set shape "farm2"
      ] if farmPop >= (maxFarm * 7 / 10 ) and farmPop < (maxFarm * 8 / 10 ) [
            set shape "farm3"
      ] if farmPop >= (maxFarm * 6 / 10 ) and farmPop < (maxFarm * 7 / 10) [
            set shape "farm4"
      ] if farmPop >=(maxFarm * 5 / 10 ) and farmPop < (maxFarm * 6 / 10 ) [
            set shape "farm5"
      ] if farmPop >= (maxFarm * 4 / 10 ) and farmPop < (maxFarm * 5 / 10 ) [
            set shape "farm6"
      ] if farmPop >= (maxFarm * 3 / 10) and farmPop < (maxFarm * 4 / 10 ) [
            set shape "farm7"
      ]  if farmPop >= (maxFarm * 2 / 10) and farmPop < (maxFarm * 3 / 10 ) [
            set shape "farm8"
      ] if farmPop > (maxFarm * 1.5 / 10 ) and farmPop < (maxFarm * 2 / 10 ) [
            set shape "farm9"
      ] if farmPop >= (maxFarm * 1 / 10 ) and farmPop < (maxFarm * 1.5 / 10 ) [
            set shape "farm10"
      ] if farmPop < (maxFarm * 1 / 10 ) [
            set shape "squareb"
     ]
    ]
   ]

  if doIHunt? [
      ask preys-here  [
        if habitat = 1 and pop > (maxPop * 7 / 8 )  [
          set shape "square2"
        ] if pop >= (maxPop * 6 / 8 ) and pop < (maxPop * 7 / 8 ) [
          set shape "square3"
        ] if pop >= (maxPop * 5 / 8 ) and pop < (maxPop * 6 / 8 ) [
          set shape "square4"
        ] if pop >= (maxPop * 4 / 8 ) and pop < (maxPop * 5 / 8 ) [
          set shape "square5"
        ] if pop >=(maxPop * 3 / 8 ) and pop < (maxPop * 4 / 8 ) [
          set shape "square6"
        ] if pop >= (maxPop * 2 / 8 ) and pop < (maxPop * 3 / 8 ) [
          set shape "square7"
        ] if pop >= (maxPop * 1.5 / 8 ) and pop < (maxPop * 2 / 8 ) [
          set shape "square8"
        ] if pop >= (maxPop * 1 / 8 ) and pop < (maxPop * 1.5 / 8 ) [
          set shape "square9"
        ] if pop < (maxPop * 1 / 8 ) [
          set shape "squareb"
        ]
      ]
    ]

end
@#$#@#$#@
GRAPHICS-WINDOW
192
25
749
583
-1
-1
26.143
1
10
1
1
1
0
0
0
1
-10
10
-10
10
1
1
1
Turns
1.0

BUTTON
12
55
73
91
go
go
T
1
T
OBSERVER
NIL
C
NIL
NIL
1

BUTTON
13
14
79
47
setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

PLOT
748
54
1316
198
Population Dynamics
Time(year)
Counts
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Individual Total" 1.0 0 -13840069 true "" "plot sum [individuals] of bands / 100"
"Total Bands" 1.0 0 -13345367 true "" "plot count bands"

MONITOR
748
10
816
55
Total Bands
count bands
2
1
11

PLOT
748
319
1315
439
Average Prey Population
Time(year)
Population
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Lush Prey Pop" 1.0 0 -2674135 true "" "plot (mean [pop] of preys with [habitat = 1]) / sqKM"
"Medium Prey Pop" 1.0 0 -4699768 true "" "plot (mean [pop] of preys with [habitat = 2]) / sqKM"
"Desert Prey Pop" 1.0 0 -1184463 true "" "plot (mean [pop] of preys with [habitat = 3]) / sqKM"

MONITOR
815
10
905
55
Total Individuals
sum [individuals] of bands
2
1
11

SWITCH
20
238
131
271
doFarm?
doFarm?
0
1
-1000

PLOT
748
436
1316
571
 Cereal & Farm Population
Time (year)
Population
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Cereal Pop. in Lush" 1.0 0 -13840069 true "" "plot mean [pop] of cereals with [habitat = 1] / 300"
"Cereal Pop. in Medium" 1.0 0 -10899396 true "" "plot mean [pop] of cereals with [habitat = 2] / 300"
"Cereal Pop. in Desert" 1.0 0 -4079321 true "" "plot mean [pop] of cereals with [habitat = 3] / 300"
"Farm Pop in Lush" 1.0 0 -11221820 true "" "plot mean [farmPop] of farms with [habitat = 1] / 300"
"Farm Pop. in Medium" 1.0 0 -13791810 true "" "plot mean [farmPop] of farms with [habitat = 2] / 300"
"Farm Pop in Desert" 1.0 0 -2064490 true "" "plot mean [farmPop] of farms with [habitat = 3] / 300"

MONITOR
1055
10
1145
55
Avg.Cereal Pop.
averageCereal
1
1
11

MONITOR
1146
10
1240
55
Avg.Prey Pop.
mean [pop] of preys / 300
1
1
11

TEXTBOX
262
10
412
28
Lush Habitat
12
52.0
1

TEXTBOX
417
10
567
28
Medium Habitat
12
53.0
1

TEXTBOX
592
11
742
29
Desert Habitat
12
53.0
1

PLOT
748
199
1314
319
Agricultural Plots 2
Time (year)
Counts
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Death Total" 1.0 0 -16777216 true "" "plot deaths"
"Avg.Prey timeLeftx1K" 1.0 0 -5825686 true "" "plot mean [tLeft] of preys / 1000"
"Avg.Cereal timeLeftx1K" 1.0 0 -13840069 true "" "plot mean [tLeft] of cereals / 1000"
"Avg.Farm timeLeftx1K" 1.0 0 -2674135 true "" "plot mean [tLeft] of farms / 1000"

MONITOR
1225
10
1317
55
Avg.Farm Pop.
mean [farmPop] of farms / 300
1
1
11

SLIDER
7
127
185
160
individualsAtStart
individualsAtStart
20
100
20.0
1
1
NIL
HORIZONTAL

SWITCH
20
276
131
309
doIHunt?
doIHunt?
0
1
-1000

BUTTON
13
390
127
423
EXPERIMENT-1
set doIHunt? false\nset doFarm? false
NIL
1
T
OBSERVER
NIL
1
NIL
NIL
1

BUTTON
9
466
123
499
EXPERIMENT-2
set doIHunt? true\nset doFarm? false
NIL
1
T
OBSERVER
NIL
2
NIL
NIL
1

TEXTBOX
15
357
165
389
Experiment-1\nBands only gather food
13
0.0
1

TEXTBOX
13
433
163
465
Experiment-2\nBands hunt and gather
13
0.0
1

TEXTBOX
11
511
197
559
Experiment-3\nBands hunt, gather and farm
13
0.0
1

BUTTON
9
544
123
577
EXPERIMENT-3
set doIHunt? true\nset doFarm? true
NIL
1
T
OBSERVER
NIL
3
NIL
NIL
1

PLOT
1319
55
1519
199
Hours Gathered Per Day
Time (year)
 Hours
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"Hours" 1.0 0 -13840069 true "" "plot mean [timegathered] of bands / 365 / 14 "

PLOT
1317
200
1517
320
Hours Hunted Per Day
Time (year) 
Hours
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"Hours" 1.0 0 -13840069 true "" "plot mean [timehunted] of bands with [habitat = 1] / 365 / 14 "
"pen-1" 1.0 0 -13345367 true "" "plot mean [timehunted] of bands with [habitat = 2] / 365 / 14 "
"pen-2" 1.0 0 -2674135 true "" "plot mean [timehunted] of bands with [habitat = 3] / 365 / 14 "

PLOT
1317
320
1517
440
Hours Farmed Per Day
Time (year)
Hours
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"Hours" 1.0 0 -13345367 true "" "plot mean [timefarmed] of bands / 365 / 14 "

MONITOR
1318
10
1411
55
Total Migrated
migratedCount
0
1
11

SLIDER
7
163
185
196
learningFactor
learningFactor
0
2
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
8
201
184
234
degradationFactor
degradationFactor
0.1
2
1.0
0.01
1
NIL
HORIZONTAL

BUTTON
89
14
152
47
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
12
591
338
762
Human population density
Time (year)
Population Density
0.0
10.0
0.0
0.6
true
true
"" ""
PENS
"Lush" 1.0 0 -13840069 true "" "plot sum [individuals] of bands with [habitat = 1] / (count patches / 3 * sqKM)"
"Medium" 1.0 0 -13345367 true "" "plot sum [individuals] of bands with [habitat = 2] / (count patches / 3 * sqKM)"
"Desert" 1.0 0 -2674135 true "" "plot sum [individuals] of bands with [habitat = 3] / (count patches / 3 * sqKM)"

MONITOR
908
10
965
55
Births
births
17
1
11

MONITOR
979
10
1036
55
Deaths
deaths
17
1
11

PLOT
1320
443
1520
593
Hours Worked Per Day
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [timefarmed + timegathered + timehunted] of bands / 365 / 14"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

farm1
false
0
Rectangle -7500403 true true 150 15 195 270

farm10
false
0
Rectangle -7500403 true true 150 240 195 270

farm2
false
0
Rectangle -7500403 true true 150 30 195 270

farm3
false
0
Rectangle -7500403 true true 150 45 195 270

farm4
false
0
Rectangle -7500403 true true 150 60 195 270

farm5
false
0
Rectangle -7500403 true true 150 90 195 270

farm6
false
0
Rectangle -7500403 true true 150 120 195 270

farm7
false
0
Rectangle -7500403 true true 150 150 195 270

farm8
false
0
Rectangle -7500403 true true 150 180 195 270

farm9
false
0
Rectangle -7500403 true true 150 210 195 270

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

square1
false
0
Rectangle -7500403 true true 90 30 210 270

square1_
false
0
Rectangle -7500403 true true 90 45 210 270

square2
false
0
Rectangle -7500403 true true 90 75 210 270

square3
false
0
Rectangle -7500403 true true 90 90 210 270

square4
false
0
Rectangle -7500403 true true 90 120 210 270

square5
false
0
Rectangle -7500403 true true 90 135 210 270

square6
false
0
Rectangle -7500403 true true 90 165 210 270

square7
false
0
Rectangle -7500403 true true 90 195 210 270

square8
false
0
Rectangle -7500403 true true 90 210 210 270

square9
false
0
Rectangle -7500403 true true 90 240 210 270

squareb
false
0
Rectangle -7500403 true true 90 270 210 270

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
