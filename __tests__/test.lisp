(defn isoDateString? [$data]
  (if-let [m (match (regexp "^(\d{4})-(\d{2})-(\d{2})$") $data)]
    (let
      [
        year (number (m 1))
        month (number (m 2))
        day (number (m 3))
        leapYear
          (and
            (zero? (mod year 4))
            (or
              (not (zero? (mod year 100)))
              (zero? (mod year 400))
            )
          )
      ]
      leapYear
    )
  )
)

(defn isoDateString? [$data]
  (if-let [m (match (regexp "^(\d{4})-(\d{2})-(\d{2})$") $data)]
    (let
      [
        year (number (m 1))
        month (number (m 2))
        day (number (m 3))
        leapYear
          (and
            (zero? (mod year 4))
            (or
              (not (zero? (mod year 100)))
              (zero? (mod year 400))
            )
          )
      ]
      (not (or
        (or (< year 1900) (> year 2100))
        (or (< month 1) (> month 12))
        (or (< day 1) (> day 31))
        (and
          (or (= month 4) (= month 6) (= month 9) (= month 11))
          (> day 30)
        )
        (and
          (= month 2)
          (or
            (and leapYear (> day 29))
            (and (not leapYear) (> day 28))
          )
        )
      ))
    )
    false
  )
)



(defn multiSelectSetter [input data loopItemValue]
  (let [data (or data [])]
    (if input
      (if (has? data loopItemValue) data (push data loopItemValue))
      (let [index (index-of data loopItemValue)]
        ((fnil remove-at -1) index data)
      )
    )
  )
)

(defn toIsoPhoneNumber [input]
  (if (and (string? input) (count input) (not= input "("))
    (str
      "+1"
      (replace
        (replace
          (replace
            input
            (regexp "([^0-9+])" :g)
            ""
          )
          (regexp "(\\+1)" :g)
          ""
        )
        (regexp "(\\+)" :g)
        ""
      )
    )
    ""
  )
)

(defn timeToContactGetter [data loopItemValue]
  (let
    [
      data (or data [])
      allAreSelected (has-every? data ["MORNING" "AFTERNOON" "MORNING"])
    ]
    (if (= loopItemValue "ANY_TIME")
      allAreSelected
      (if allAreSelected
        false
        (has? data loopItemValue)
      )
    )
  )
)

(defn timeToContactSetter [input data loopItemValue]
  (let
    [
      data (or data [])
      allSelectedArr ["MORNING" "AFTERNOON" "MORNING"]
      allAreSelected (has-every? data allSelectedArr)
    ]
    (if input
      (if (= loopItemValue "ANY_TIME")
        allSelectedArr
        (if allAreSelected
          [loopItemValue]
          (if (has? data loopItemValue)
            data
            (push data loopItemValue)
          )
        )
      )
      (if (= loopItemValue "ANY_TIME")
        []
        (filter #(not= %1 loopItemValue) data)
      )
    )
  )
)

(def NAME_LENGTH 100)

(def RESEND_PIN_TIMEOUT 10000)

(defn zip? [string] (boolean (match (regexp "^\d{5}$") string)))

(defn email? [string] (boolean (match (regexp "^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])$") string)))

(defn isoDateString? [string] (boolean (match (regexp "^\d{4}-\d{2}-\d{2}$") string)))

(defn isoDateString? [data]  (if-let [m (match (regexp "^(\d{4})-(\d{2})-(\d{2})$") data)]    (let      [        year (number (m 1))        month (number (m 2))        day (number (m 3))        leapYear          (and            (zero? (mod year 4))            (or              (not (zero? (mod year 100)))              (zero? (mod year 400))            )          )      ]      (not (or        (or (< year 1900) (> year 2100))        (or (< month 1) (> month 12))        (or (< day 1) (> day 31))        (and          (or (= month 4) (= month 6) (= month 9) (= month 11))          (> day 30)        )        (and          (= month 2)          (or            (and leapYear (> day 29))            (and (not leapYear) (> day 28))          )        )      ))    )    false  ))

(defn formatPhoneNumber [data] (if (string? data)   (let [phoneNumber (if (= "+" (nth data 0)) (subs data 2) data)]     (cond       (> (count phoneNumber) 6)         (str           "("           (subs phoneNumber 0 3)           ") "           (subs phoneNumber 3 6)           "-"           (subs phoneNumber 6))       (> (count phoneNumber) 3)         (str "(" (subs phoneNumber 0 3) ") " (subs phoneNumber 3))       (> (count phoneNumber) 0)         (str "(" (subs phoneNumber 0))       true         phoneNumber     )   )   "" ))

(defn label-from-value [arr value] (let [entry (some #(= value (%1 :value)) arr)]    (if (nil? entry) (str value) (entry :label))  ))

(defn labels-from-values [arr values]  (for    [      value      values      &let [        label        (let [entry (some #(= value (%1 :value)) $array)]          (if (nil? entry) (str value) (entry :label))        )      ]      &when (boolean label)    ]    label  ))

(defn multiSelectSetter [input data loopItemValue]  (let [data (or data [])]    (if input      (if (has? data loopItemValue) data (push data input))      (let [index (index-of data loopItemValue)]        ((fnil remove-at -1) index data)      )    )  ))

(defn toIsoPhoneNumber [input]  (if (and (string? input) (count input) (not= input "("))    (str      "+1"      (replace        (replace          (replace            input            (regexp "([^0-9+])" :g)            ""          )          (regexp "(\+1)" :g)          ""        )        (regexp "(\+)" :g)        ""      )    )    ""  ))

(defn timeToContactGetter [data loopItemValue]  (let    [      data (or data [])      allAreSelected (has-every? data ["MORNING" "AFTERNOON" "MORNING"])    ]    (if (= loopItemValue "ANY_TIME")      allAreSelected      (if allAreSelected        false        (has? data loopItemValue)      )    )  ))

(defn timeToContactSetter [input data loopItemValue]  (let    [      data (or data [])      allSelectedArr ["MORNING" "AFTERNOON" "MORNING"]      allAreSelected (has-every? data allSelectedArr)    ]    (if input      (if (= loopItemValue "ANY_TIME")        allSelectedArr        (if allAreSelected          [loopItemValue]          (if (has? data loopItemValue)            data            (push data loopItemValue)          )        )      )      (if (= loopItemValue "ANY_TIME")        []        (filter #(not= %1 loopItemValue) data)      )    )  ))

(loop [list (entries *translations*)]
  (when (count list)
    (let [entry (first list)]
      (defns (entry 0) [&rest params &let [templateString (entry 1)]]
        (apply template (cons templateString params))
      )
      (recur (rest list))
    )
  )
)

(defn boolToStringArraySetter [input data loopItemValue]
  (let [data (or data [])]
    (if $input
      (if (has? data loopItemValue) data (push data loopItemValue))



;; $totalCdlExperienceOutdated ||
($totalCdlExperienceTotalCdl === $experienceEnum[0].value && $cdlTrainingOutdated) ||
($totalCdlExperienceTotalCdl !== $experienceEnum[0].value && ((($requiredExperiences || []).includes('COMMERCIAL_DRIVING') && !!$COMMERCIAL_DRIVING_ExperienceOutdated) ||
(($requiredExperiences || []).includes('STRAIGHT_TRUCK_TANKER') && !!$STRAIGHT_TRUCK_TANKER_ExperienceOutdated) ||
(($requiredExperiences || []).includes('STRAIGHT_TRUCK_READY_MIXER') && !!$STRAIGHT_TRUCK_READY_MIXER_ExperienceOutdated) ||
(($requiredExperiences || []).includes('TRACTOR_TRAILER') && !!$TRACTOR_TRAILER_ExperienceOutdated) ||
(($requiredExperiences || []).includes('TRACTOR_TRAILER_OTR') && !!$TRACTOR_TRAILER_OTR_ExperienceOutdated) ||
(($requiredExperiences || []).includes('TRACTOR_TRAILER_FLATBED') && !!$TRACTOR_TRAILER_FLATBED_ExperienceOutdated) ||
(($requiredExperiences || []).includes('TRACTOR_TRAILER_TANKER') && !!$TRACTOR_TRAILER_TANKER_ExperienceOutdated) ||
(($requiredExperiences || []).includes('TRACTOR_TRAILER_HEAVY_HAUL') && !!$TRACTOR_TRAILER_HEAVY_HAUL_ExperienceOutdated)))


(boolean
  (or
    $totalCdlExperienceOutdated
    (and (= $totalCdlExperienceTotalCdl (get-in $experienceEnum [0 :value])) $cdlTrainingOutdated)
    (and (!= $totalCdlExperienceTotalCdl (get-in $experienceEnum [0 :value])) (and (has? ((or $requiredExperiences [])) "COMMERCIAL_DRIVING") $COMMERCIAL_DRIVING_ExperienceOutdated))
    (and (has? ((or $requiredExperiences [])) "STRAIGHT_TRUCK_TANKER") $STRAIGHT_TRUCK_TANKER_ExperienceOutdated)
    (and (has? ((or $requiredExperiences [])) "STRAIGHT_TRUCK_READY_MIXER") $STRAIGHT_TRUCK_READY_MIXER_ExperienceOutdated)
    (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER") $TRACTOR_TRAILER_ExperienceOutdated)
    (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER_OTR") $TRACTOR_TRAILER_OTR_ExperienceOutdated)
    (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER_FLATBED") $TRACTOR_TRAILER_FLATBED_ExperienceOutdated)
    (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER_TANKER") $TRACTOR_TRAILER_TANKER_ExperienceOutdated)
    (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER_HEAVY_HAUL") $TRACTOR_TRAILER_HEAVY_HAUL_ExperienceOutdated)
    (and (has? ((or $requiredExperiences [])) "COMMERCIAL_DRIVING") $COMMERCIAL_DRIVING_ExperienceOutdated)
    (and (has? ((or $requiredExperiences [])) "COMMERCIAL_DRIVING") $COMMERCIAL_DRIVING_ExperienceOutdated)
  )
)

(boolean (or $totalCdlExperienceOutdated (and (= $totalCdlExperienceTotalCdl (get-in $experienceEnum [0 :value])) $cdlTrainingOutdated) (and (!= $totalCdlExperienceTotalCdl (get-in $experienceEnum [0 :value])) (and (has? ((or $requiredExperiences [])) "COMMERCIAL_DRIVING") $COMMERCIAL_DRIVING_ExperienceOutdated)) (and (has? ((or $requiredExperiences [])) "STRAIGHT_TRUCK_TANKER") $STRAIGHT_TRUCK_TANKER_ExperienceOutdated) (and (has? ((or $requiredExperiences [])) "STRAIGHT_TRUCK_READY_MIXER") $STRAIGHT_TRUCK_READY_MIXER_ExperienceOutdated) (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER") $TRACTOR_TRAILER_ExperienceOutdated) (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER_OTR") $TRACTOR_TRAILER_OTR_ExperienceOutdated) (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER_FLATBED") $TRACTOR_TRAILER_FLATBED_ExperienceOutdated) (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER_TANKER") $TRACTOR_TRAILER_TANKER_ExperienceOutdated) (and (has? ((or $requiredExperiences [])) "TRACTOR_TRAILER_HEAVY_HAUL") $TRACTOR_TRAILER_HEAVY_HAUL_ExperienceOutdated) (and (has? ((or $requiredExperiences [])) "COMMERCIAL_DRIVING") $COMMERCIAL_DRIVING_ExperienceOutdated) (and (has? ((or $requiredExperiences [])) "COMMERCIAL_DRIVING") $COMMERCIAL_DRIVING_ExperienceOutdated)))

(str "Owner Operator" (if (= $ownerOperatorApplyAs $applyAsOwnerOperatorValue) "" " (Lease Purchase)"))

(defn labels-from-values [arr values $opt default]
  (let
    [
      labels
      (for
        [
          value
          values
          &let [
            label
            (let [entry (some #(= value (%1 :value)) $array)]
              (if (nil? entry) (str value) (entry :label))
            )
          ]
          &when (boolean label)
        ]
        label
      )
    ]
    (if (and (string? default) (empty? labels)) [default] labels)
)

(defn labels-from-values [arr values $opt default]
  (let
    [
      labels
      (for
        [
          value
          values
          &let [
            label
            (let [entry (some #(= value (%1 :value)) $array)]
              (if (nil? entry) (str value) (entry :label))
            )
          ]
          &when (boolean label)
        ]
        label
      )
    ]
    (if (and (string? default) (empty? labels)) [default] labels)
)



(def NAME_LENGTH 100)
(def RESEND_PIN_TIMEOUT 10000)
(defn zip? [string] (boolean (match (regexp "^\d{5}$") string)))
(defn email? [string] (boolean (match (regexp "^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])$") string)))
(defn isoDateString? [data]  (if-let [m (match (regexp "^(\d{4})-(\d{2})-(\d{2})$") data)]    (let      [        year (number (m 1))        month (number (m 2))        day (number (m 3))        leapYear          (and            (zero? (mod year 4))            (or              (not (zero? (mod year 100)))              (zero? (mod year 400))            )          )      ]      (not (or        (or (< year 1900) (> year 2100))        (or (< month 1) (> month 12))        (or (< day 1) (> day 31))        (and          (or (= month 4) (= month 6) (= month 9) (= month 11))          (> day 30)        )        (and          (= month 2)          (or            (and leapYear (> day 29))            (and (not leapYear) (> day 28))          )        )      ))    )    false  ))
(defn formatPhoneNumber [data] (if (string? data)   (let [phoneNumber (if (= "+" (nth data 0)) (subs data 2) data)]     (cond       (> (count phoneNumber) 6)         (str           "("           (subs phoneNumber 0 3)           ") "           (subs phoneNumber 3 6)           "-"           (subs phoneNumber 6))       (> (count phoneNumber) 3)         (str "(" (subs phoneNumber 0 3) ") " (subs phoneNumber 3))       (> (count phoneNumber) 0)         (str "(" (subs phoneNumber 0))       true         phoneNumber     )   )   "" ))
(defn label-from-value [arr value]  (let [entry (some #(= value (%1 :value)) arr)]    (if (nil? entry) (str value) (entry :label))  ))
(defn labels-from-values [arr values $opt default](let  [    labels    (for      [        value        values        &let [          label          (let [entry (some #(= value (%1 :value)) $array)]            (if (nil? entry) (str value) (entry :label))          )        ]        &when (boolean label)      ]      label    )  ]  (if (and (string? default) (empty? labels)) [default] labels)))
(defn multiSelectSetter [input data loopItemValue]  (let [data (or data [])]    (if input      (if (has? data loopItemValue) data (push data loopItemValue))      (let [index (index-of data loopItemValue)]        ((fnil remove-at -1) index data)      )    )  ))
(defn toIsoPhoneNumber [input]  (if (and (string? input) (count input) (not= input "("))    (str      "+1"      (replace        (replace          (replace            input            (regexp "([^0-9+])" :g)            ""          )          (regexp "(\+1)" :g)          ""        )        (regexp "(\+)" :g)        ""      )    )    ""  ))
(defn timeToContactGetter [data loopItemValue]  (let    [      data (or data [])      allAreSelected (has-every? data ["MORNING" "AFTERNOON" "MORNING"])    ]    (if (= loopItemValue "ANY_TIME")      allAreSelected      (if allAreSelected        false        (has? data loopItemValue)      )    )  ))
(defn timeToContactSetter [input data loopItemValue]  (let    [      data (or data [])      allSelectedArr ["MORNING" "AFTERNOON" "MORNING"]      allAreSelected (has-every? data allSelectedArr)    ]    (if input      (if (= loopItemValue "ANY_TIME")        allSelectedArr        (if allAreSelected          [loopItemValue]          (if (has? data loopItemValue)            data            (push data loopItemValue)          )        )      )      (if (= loopItemValue "ANY_TIME")        []        (filter #(not= %1 loopItemValue) data)      )    )  ))
