globals [generation payoffs strategies fit_noone fit_altruistic fit_spiteful fit_antisocial fit_altruistic-3P fit_antisocial-3P fighters_antisoc fighters_altruist neutral all_obs b1 b2 pun1 pun2 gpun1 gpun2 running-s ratio coal1 coal2 DDs CDs CCs p_fights p_dominations p_deterred g_fights g_dominations g_deterred]

breed [agents agent]

agents-own [partnered observe justified decision fitness strength feelstrong strategy fear blacklist neutrals antisoc_count altruist_count norm]

to setup
  clear-all
  set payoffs [[6 0][10 2]] ;cooperate: 0, defect: 1
  set strategies ["noone" "altruistic" "spiteful" "antisocial" "altruistic-3P" "antisocial-3P"]
  set generation 1
  set fighters_antisoc []
  set fighters_altruist []
  set neutral []
  set all_obs []
  create num_noone num_altruistic num_spiteful num_antisocial num_altruistic_3P num_antisocial_3P
  agent-look
  reset-ticks
end

; helper for creating agents with different strategies
to create [noone altruistic spiteful antisocial altruistic-3P antisocial-3P]
  create-agents noone [
    set strategy item 0 strategies
    set fitness 1000
    set strength random-normal 10 1
    set fear []
    set blacklist []
    set neutrals []
    setxy random-pxcor random-pycor
    ]
  create-agents altruistic [
    set strategy item 1 strategies
    set fitness 1000
    set strength random-normal 10 1
    set fear []
    set blacklist []
    set neutrals []
    setxy random-pxcor random-pycor
    ]
  create-agents spiteful [
    set strategy item 2 strategies
    set fitness 1000
    set strength random-normal 10 1
    set fear []
    set blacklist []
    set neutrals []
    setxy random-pxcor random-pycor
    ]
  create-agents antisocial [
    set strategy item 3 strategies
    set fitness 1000
    set strength random-normal 10 1
    set fear []
    set blacklist []
    set neutrals []
    setxy random-pxcor random-pycor
    ]
  create-agents altruistic-3P [
    set strategy item 4 strategies
    set fitness 1000
    set strength random-normal 10 1
    set fear []
    set blacklist []
    set neutrals []
    set norm 1.1
    setxy random-pxcor random-pycor
    ]
  create-agents antisocial-3P [
    set strategy item 5 strategies
    set fitness 1000
    set strength random-normal 10 1
    set fear []
    set blacklist []
    set neutrals []
    set norm 0.9
    setxy random-pxcor random-pycor
    ]
end

to agent-look
  ask agents with [strategy = item 0 strategies] [
    set shape "fish"
    set color orange
    ]
  ask agents with [strategy = item 1 strategies] [
    set shape "dog"
    set color brown
    ]
  ask agents with [strategy = item 2 strategies] [
    set shape "spider"
    set color grey
    ]
  ask agents with [strategy = item 3 strategies] [
    set shape "cat"
    set color white
    ]
  ask agents with [strategy = item 4 strategies] [
    set shape "person"
    set color brown
    ]
  ask agents with [strategy = item 5 strategies] [
    set shape "person"
    set color white
    ]
end


;one interaction round among agents
to interact
  pair
  foreach sort links [
    ask ? [
      compete sensitivity coalition_sensitivity
      play
      punish sensitivity coalition_sensitivity
      ]
    ]
  ask agents [
    set norm norm-update norm 0.2 antisoc_count altruist_count
    set partnered 0
    set observe 0
    set justified 0 
    set label fitness
    ]
  clear-links
end

;helper function for pairing agents
to pair
  let total count agents
  let pairs floor (total / 10) ;1/5 of population play
  repeat pairs [
    ask one-of agents with [partnered = 0 and observe = 0] [
      create-link-with one-of other agents with [partnered = 0 and observe = 0] 
      set partnered 1 
      ask link-neighbors [set partnered 1]
      ]
  ]
end

to compete [sens coal_sens]
  let p1 end1
  let p2 end2
  let s1 [strength] of end1
  let s2 [strength] of end2
  let str1 [strategy] of end1
  let str2 [strategy] of end2
  ifelse member? p2 [fear] of end1
  [set b1 deterrence]
  [set b1 0]
  ifelse member? p1 [fear] of end2
  [set b2 deterrence]
  [set b2 0]
  let util1 sigmoid (s1 - (s2 + b1)) sens ;strength of the opponent is iverestimated if he's in fear list
  let util2 sigmoid (s2 - (s1 + b2)) sens
  ; (r < util) denotes agent thinks he's stronger
  ask end1 [
    let r random-float 1
    ifelse r < util1
    [set feelstrong 1]
    [set feelstrong 0]
    ]
  ask end2 [
    let r random-float 1
    ifelse r < util2
    [set feelstrong 1]
    [set feelstrong 0]
    ]
  ask end1 [
    if str1 = "altruistic-3P"
    [altr-decide p1 p2]
    if str1 = "antisocial-3P"
    [
      ifelse norm > 1
      [set decision 0]
      [set decision 1]
      ]
    if str1 != "antisocial-3P" and str1 != "altruistic-3P"
    [
      ifelse feelstrong = 0
      [set decision 0]
      [set decision 1]
      ]
    ]
  ask end2 [
    if str2 = "altruistic-3P"
    [altr-decide p2 p1]
    if str2 = "antisocial-3P"
    [
      ifelse norm > 1
      [set decision 0]
      [set decision 1]
      ]
    if (str2 != "antisocial-3P" and str2 != "altruistic-3P")
    [
      ifelse feelstrong = 0
      [set decision 0]
      [set decision 1]
      ]
    ]
end

to altr-decide [me enemy]
  if altr_rule = "Defect_Defectors&Neutrals"
  [
    ifelse ((member? enemy blacklist) or (member? enemy neutrals))
      [set decision 1 set justified 1]
      [set decision 0]
    ]
  if altr_rule = "Defect_Defectors"
  [
    ifelse (member? enemy blacklist)
      [set decision 1 set justified 1]
      [set decision 0]
    ]
  if altr_rule = "Norm_Only"
  [set decision 0]
end

;helper function for the PD game
to play
  if [decision] of end1 = 0 and [decision] of end2 = 0
  [set CCs (CCs + 1)]
  if [decision] of end1 = 1 and [decision] of end2 = 1
  [set DDs (DDs + 1)]
  if ([decision] of end1 = 0 and [decision] of end2 = 1) or ([decision] of end1 = 1 and [decision] of end2 = 0)
  [set CDs (CDs + 1)]
  let add1 item [decision] of end2 item [decision] of end1 payoffs
  ask end1 [set fitness fitness + add1]
  let add2 item [decision] of end1 item [decision] of end2 payoffs
  ask end2 [set fitness fitness + add2]
end

;helper function for punishment
to punish [sens coal_sens]
  let dec1 [decision] of end1
  let dec2 [decision] of end2
  let str1 [strategy] of end1
  let str2 [strategy] of end2
  let s1 [strength] of end1
  let s2 [strength] of end2
  let j1 [justified] of end1
  let j2 [justified] of end2
  build-coalition
  let antisoc length fighters_antisoc
  let altr length fighters_altruist
  ; determine conflict escalation
  ;private fight - like in model1, where there is noone to intervene
  ifelse (antisoc = 0 and altr = 0)
  [private-fight sens]
  [
    ifelse ((dec1 = 1 and dec2 = 1) or (dec1 = 1 and dec2 = 0 and j1 = 1) or (dec1 = 0 and dec2 = 1 and j2 = 1))
    [coal-fight-DD sens coal_sens]
    [coal-fight coal_sens]
    ]
  ;update estimates
  ask end1 [set antisoc_count (antisoc_count + antisoc)]
  ask end1 [set altruist_count (altruist_count + altr)]
  ask end2 [set antisoc_count (antisoc_count + antisoc)]
  ask end2 [set altruist_count (altruist_count + altr)]
end

to private-fight [sens]
  set pun1 0 ;retreat: 0, fight: 1
  set pun2 0
  let dec1 [decision] of end1
  let dec2 [decision] of end2
  let str1 [strategy] of end1
  let str2 [strategy] of end2
  let s1 [strength] of end1
  let s2 [strength] of end2
  let p1 end1
  let p2 end2
    if dec1 = 1 and dec2 = 1 ;DD case
  [
    if (str1 = "altruistic" or str1 = "spiteful" or str1 = "altruistic-3P")
    [set pun1 1]
    if (str2 = "altruistic" or str2 = "spiteful" or str2 = "altruistic-3P")
    [set pun2 1]
    ]
  if dec1 = 1 and dec2 = 0 ;DC case
  [
    if (str1 = "antisocial" or str1 = "spiteful" or str1 = "antisocial-3P")
    [set pun1 1]
    ]
  if dec1 = 0 and dec2 = 1 ;CD case
  [
    if (str2 = "antisocial" or str2 = "spiteful" or str2 = "antisocial-3P")
    [set pun2 1]
    ]
  ; nothing happens in case of CC since both agents fear each other
  if (pun1 = 1 and pun2 = 1) ;fight!!
  [
    set p_fights (p_fights + 1)
    let util1 (sigmoid (s1 - s2) sens) ;here the true fighting ability matters
    ifelse random-float 1 < util1 
    [ ;agent end1 wins
      ask end1 [
        let change (fitness - pun_cost)
        ifelse change > 0
        [set fitness change]
        [die]
        ] 
      ask end2 [
        set fear fear_update p1 fear
        if (dec1 = 1 and str2 = "altruistic-3P")
        [set blacklist fear_update p1 blacklist]
        let change (fitness - punishment)
        ifelse change > 0
        [set fitness change]
        [die]
        ]
      foreach all_obs [
        let obs-str [strategy] of ?
        ask ? [set fear fear_update p1 fear]
        if dec1 = 1 and (obs-str = "altruistic-3P")
        [ask ? [set blacklist fear_update p1 blacklist]]
        ask n-of gossip agents [
          set fear fear_update p1 fear
          if (dec1 = 1 and obs-str = "altruistic-3P" and strategy = "altruistic-3P")
          [set blacklist fear_update p1 blacklist]
          ]
        ]
      ]
    [ ;agent end2 wins
      ask end1 [
        set fear fear_update p2 fear
        if (dec2 = 1 and str1 = "altruistic-3P")
        [set blacklist fear_update p2 blacklist]
        let change (fitness - punishment)
        ifelse change > 0
        [set fitness change]
        [die]
        ]
       foreach all_obs [
        let obs-str [strategy] of ?
        ask ? [set fear fear_update p2 fear]
        if (dec2 = 1 and obs-str = "altruistic-3P")
        [ask ? [set blacklist fear_update p2 blacklist]]
        ask n-of gossip agents [
          set fear fear_update p2 fear
          if (dec2 = 1 and obs-str = "altruistic-3P" and strategy = "altruistic-3P")
          [set blacklist fear_update p2 blacklist]
          ]
        ]
      ask end2 [
        let change (fitness - pun_cost)
        ifelse change > 0
        [set fitness change]
        [die]
        ]
      ]
    ]
  if (pun1 = 1 and pun2 = 0)
  [
    set p_dominations (p_dominations + 1)
    foreach all_obs [
        let obs-str [strategy] of ?
        ask ? [set fear fear_update p1 fear]
        if (dec1 = 1 and obs-str = "altruistic-3P")
        [ask ? [set blacklist fear_update p1 blacklist]]
        ask n-of gossip agents [
          set fear fear_update p1 fear
          if (dec1 = 1 and obs-str = "altruistic-3P" and strategy = "altruistic-3P")
          [set blacklist fear_update p1 blacklist]
          ]
        ] 
    ask end2 [
      set fear fear_update p1 fear
        if (dec1 = 1 and str2 = "altruistic-3P")
        [set blacklist fear_update p1 blacklist]
        let change (fitness - submit_cost)
        ifelse change > 0
        [set fitness change]
        [die]
        ]
    ]
  if (pun1 = 0 and pun2 = 1)
  [
    set p_dominations (p_dominations + 1)
    foreach all_obs [
        let obs-str [strategy] of ?
        ask ? [set fear fear_update p2 fear]
        if (dec2 = 1 and obs-str = "altruistic-3P")
        [ask ? [set blacklist fear_update p2 blacklist]]
        ask n-of gossip agents [
          set fear fear_update p2 fear
          if (dec2 = 1 and obs-str = "altruistic-3P" and strategy = "altruistic-3P")
          [set blacklist fear_update p2 blacklist]
          ]
        ]
    ask end1 [
      set fear fear_update p2 fear
        if (dec2 = 1 and str1 = "altruistic-3P")
        [set blacklist fear_update p2 blacklist]
        let change (fitness - submit_cost)
        ifelse change > 0
        [set fitness change]
        [die]
        ]
  ]
  if (pun1 = 0 and pun2 = 0)
  [set p_deterred (p_deterred + 1)]
end

;Coalition fight
to coal-fight [coal_sens]
  let dec1 [decision] of end1
  let dec2 [decision] of end2
  let p1 end1
  let p2 end2
  let j1 [justified] of end1
  let j2 [justified] of end2
  if (dec1 = 0 and dec2 = 0) [set g_deterred (g_deterred + 1)]
  if (dec1 = 1 and dec2 = 0) ;DC case
    [
      set fighters_antisoc fput p1 fighters_antisoc
      set fighters_altruist fput p2 fighters_altruist
      set coal1 coal-strength fighters_antisoc
      set coal2 coal-strength fighters_altruist
      let util1 (sigmoid (coal1 - coal2) coal_sens)
      let util2 (sigmoid (coal2 - coal1) coal_sens)
      ifelse random-float 1 < util1
      [set gpun1 1]
      [set gpun1 0]
      ifelse random-float 1 < util2
      [set gpun2 1]
      [set gpun2 0]
      ;ifelse random-float 1 > util1 ;coalition of agent end2 wins
      if gpun1 = 0 and gpun2 = 0
      [set g_deterred (g_deterred + 1)]
      if gpun1 = 0 and gpun2 = 1 ;altr punish defector
        [
          set g_dominations (g_dominations + 1)
          foreach fighters_altruist [ask ? [
              let cost (pun_cost / coal2 ^ 2)
              let change (fitness - cost)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]]
          foreach fighters_antisoc [
            ask ? [
              foreach fighters_altruist [set fear fear_update ? fear]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]
            ask n-of gossip agents [
              foreach fighters_altruist [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [foreach neutral [set neutrals fear_update ? neutrals]]
              ]
          ]
          foreach neutral [
            ask ? [
              foreach fighters_altruist [set fear fear_update ? fear]
              ]
            ask n-of gossip agents [
              foreach fighters_altruist [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [foreach neutral [set neutrals fear_update ? neutrals]]
              ]
          ]
          ask end1 [
            let change (fitness - punishment)
              ifelse change > 0
              [set fitness change]
              [die]
            ]
          ]
      if gpun1 = 1 and gpun2 = 0 ;antis punish cooperator
        [
          set g_dominations (g_dominations + 1)
          foreach fighters_antisoc [ask ? [
              let cost (pun_cost / coal1 ^ 2)
              let change (fitness - cost)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]]
          foreach fighters_altruist [
            ask ? [
              foreach fighters_antisoc [
                set fear fear_update ? fear
                set blacklist fear_update ? blacklist
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
            ask n-of gossip agents [
              foreach fighters_antisoc [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
          ]
          foreach neutral [
            ask ? [
              foreach fighters_antisoc [set fear fear_update ? fear]
              ]
            ask n-of gossip agents [
              foreach fighters_antisoc [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
          ]
          ask end2 [
            let change (fitness - punishment)
              ifelse change > 0
              [set fitness change]
              [die]
            ]
          ]
        if gpun1 = 1 and gpun2 = 1
        [
          set g_fights (g_fights + 1)
          ifelse random-float 1 < util1 ;coal1 wins - antis punish cooperator
          [
            foreach fighters_antisoc [ask ? [
              let cost (pun_cost / (coal2 / coal1) ^ 2)
              let change (fitness - cost)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]]
          foreach fighters_altruist [
            ask ? [
              let change (fitness - punishment)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach fighters_antisoc [
                set fear fear_update ? fear
                set blacklist fear_update ? blacklist
                ]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]
            ask n-of gossip agents [
              foreach fighters_antisoc [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
          ]
          foreach neutral [
            ask ? [
              foreach fighters_antisoc [set fear fear_update ? fear]
              ]
            ask n-of gossip agents [
              foreach fighters_antisoc [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
          ]
            ]
          [ ;altrs wins
            foreach fighters_altruist [ask ? [
              let cost (pun_cost / (coal1 / coal2) ^ 2)
              let change (fitness - cost)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]]
          foreach fighters_antisoc [
            ask ? [
              let change (fitness - punishment)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach fighters_altruist [set fear fear_update ? fear]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]
            ask n-of gossip agents [
              foreach fighters_altruist [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [foreach neutral [set neutrals fear_update ? neutrals]]
              ]
          ]
          foreach neutral [
            ask ? [
              foreach fighters_altruist [set fear fear_update ? fear]
              ]
            ask n-of gossip agents [
              foreach fighters_altruist [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [foreach neutral [set neutrals fear_update ? neutrals]]
              ]
          ]
            ]
          ]
    ]
    if dec1 = 0 and dec2 = 1 ;CD case
    [
      set fighters_antisoc fput p2 fighters_antisoc
      set fighters_altruist fput p1 fighters_altruist
      set coal2 coal-strength fighters_antisoc
      set coal1 coal-strength fighters_altruist
      let util1 (sigmoid (coal1 - coal2) coal_sens)
      let util2 (sigmoid (coal2 - coal1) coal_sens)
      ifelse random-float 1 < util1
      [set gpun1 1]
      [set gpun1 0]
      ifelse random-float 1 < util2
      [set gpun2 1]
      [set gpun2 0]
      ;ifelse random-float 1 < util1 ;coalition of agent end1 wins
      if gpun1 = 0 and gpun2 = 0
      [set g_deterred (g_deterred + 1)]
      if gpun1 = 1 and gpun2 = 0 ;altrs punish defector
        [
          set g_dominations (g_dominations + 1)
          foreach fighters_altruist [ask ? [
              let cost (pun_cost / coal1 ^ 2)
              let change (fitness - cost)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]]
          foreach fighters_antisoc [
            ask ? [
              foreach fighters_altruist [set fear fear_update ? fear]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]
            ask n-of gossip agents [
              foreach fighters_altruist [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [foreach neutral [set neutrals fear_update ? neutrals]]
              ]
          ]
          foreach neutral [
            ask ? [
              foreach fighters_altruist [set fear fear_update ? fear]
              ]
            ask n-of gossip agents [
              foreach fighters_altruist [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [foreach neutral [set neutrals fear_update ? neutrals]]
              ]
            ]
          ask end2 [
            let change (fitness - punishment)
              ifelse change > 0
              [set fitness change]
              [die]
            ]
          ]
      if gpun1 = 0 and gpun2 = 1 ;antisocs punish cooperator
        [
          set g_dominations (g_dominations + 1)
          foreach fighters_antisoc [ask ? [
              let cost (pun_cost / coal2 ^ 2)
              let change (fitness - cost)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]]
          foreach fighters_altruist [
            ask ? [
              foreach fighters_antisoc [
                set fear fear_update ? fear
                set blacklist fear_update ? blacklist
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
            ask n-of gossip agents [
              foreach fighters_antisoc [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
            ]
          foreach neutral [
            ask ? [
              foreach fighters_antisoc [set fear fear_update ? fear]
              ]
            ask n-of gossip agents [
              foreach fighters_antisoc [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
            ]
          ask end1 [
            let change (fitness - punishment)
              ifelse change > 0
              [set fitness change]
              [die]
            ]
          ]
        if gpun1 = 1 and gpun2 = 1
        [
          set g_fights (g_fights + 1)
          ifelse random-float 1 < util1 ;coal1 wins - altrs punish defector
          [
            foreach fighters_altruist [ask ? [
              let cost (pun_cost / (coal2 / coal1) ^ 2)
              let change (fitness - cost)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]]
          foreach fighters_antisoc [
            ask ? [
              let change (fitness - punishment)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach fighters_altruist [set fear fear_update ? fear]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]
            ask n-of gossip agents [
              foreach fighters_altruist [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
          ]
          foreach neutral [
            ask ? [
              foreach fighters_altruist [set fear fear_update ? fear]
              ]
            ask n-of gossip agents [
              foreach fighters_altruist [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [foreach neutral [set neutrals fear_update ? neutrals]]
              ]
            ]
            ]
          [
            foreach fighters_antisoc [ask ? [
              let cost (pun_cost / (coal1 / coal2) ^ 2)
              let change (fitness - cost)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach neutral [set neutrals fear_update ? neutrals]
              ]]
          foreach fighters_altruist [
            ask ? [
              let change (fitness - punishment)
              ifelse change > 0
              [set fitness change]
              [die]
              foreach fighters_antisoc [
                set fear fear_update ? fear
                set blacklist fear_update ? blacklist
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
            ask n-of gossip agents [
              foreach fighters_antisoc [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
            ]
          foreach neutral [
            ask ? [
              foreach fighters_antisoc [set fear fear_update ? fear]
              ]
            ask n-of gossip agents [
              foreach fighters_antisoc [set fear fear_update ? fear]
              if strategy = "altruistic-3P"
              [
                foreach fighters_antisoc [set blacklist fear_update ? blacklist]
                foreach neutral [set neutrals fear_update ? neutrals]
                ]
              ]
            ]
            ]
          ]
      ]
end

to coal-fight-DD [sens coal_sens]
  let p1 end1
  let p2 end2
  let j1 [justified] of end1
  let j2 [justified] of end2
  if (j1 = 0 and j2 = 0) or (j1 = 1 and j2 = 1)
  [private-fight sens]
  if j1 = 1 and j2 = 0
  [
    ask end1 [set decision 0]
    move-agents
    coal-fight coal_sens
    ]
  if j1 = 0 and j2 = 1
  [
    ask end2 [set decision 0]
    move-agents
    coal-fight coal_sens
    ]
end

to build-coalition
  set fighters_antisoc []
  set fighters_altruist []
  set neutral []
  set all_obs []
  ask n-of 8 agents with [partnered = 0 and observe = 0] [
    if strategy = "antisocial-3P"
    [ 
      set fighters_antisoc fput self fighters_antisoc
      ]
    if strategy = "altruistic-3P"
    [
      set fighters_altruist fput self fighters_altruist
      ]
    if (strategy = "noone" or strategy = "altruistic" or strategy = "spiteful" or strategy = "antisocial")
    [
      set neutral fput self neutral
      ]
    set all_obs fput self all_obs
    set observe 1
    ]
end

to move-agents
  set neutral sentence fighters_antisoc neutral
  set fighters_antisoc []
end

to-report coal-strength [rival]
  set running-s 0
  foreach rival [
    let add [strength] of ?
    set running-s (running-s + add)
    ]
  report running-s
end

to-report sigmoid [diff sens]
  let val 1 / (1 + exp(- diff * sens))
  report val
end

to-report fear_update [new_agent fearl]
  ifelse not member? new_agent fearl
  [
    ifelse (length fearl < memory)
  [
    let fearlist fput new_agent fearl
    report fearlist
    ]
  [
    let new_fearlist but-last fearl
    let fearlist fput new_agent new_fearlist
    report fearlist
    ] 
    ]
  [report fearl]
end

to-report norm-update [oldval alfa antisocials altruists]
  set ratio 0
  ifelse (antisocials != 0 and altruists != 0)
  [set ratio (altruists / (antisocials))]
  [
    if (antisocials = 0 and altruists = 0)
    [set ratio 1]
    if (antisocials != 0 and altruists = 0)
    [set ratio 1 / antisocials]
    if (antisocials = 0 and altruists != 0)
    [set ratio altruists]
    ]
  let newval (((1 - alfa) * oldval) + (alfa * ratio))
  report newval
end
; transition from one generation to another
to replicate
  let current-norm mean [norm] of agents
  let total count agents
  ;noone
  let no_noone count agents with [strategy = item 0 strategies]
  set fit_noone 0
  if no_noone > 0
  [set fit_noone mean [fitness] of agents with [strategy = item 0 strategies]]
  let weight_noone (no_noone / total) * fit_noone
  ;altruistic
  let no_altruistic count agents with [strategy = item 1 strategies]
  set fit_altruistic 0
  if no_altruistic > 0
  [set fit_altruistic mean [fitness] of agents with [strategy = item 1 strategies]]
  let weight_altruistic (no_altruistic / total) * fit_altruistic
  ;spiteful
  let no_spiteful count agents with [strategy = item 2 strategies]
  set fit_spiteful 0
  if no_spiteful > 0
  [set fit_spiteful mean [fitness] of agents with [strategy = item 2 strategies]]
  let weight_spiteful (no_spiteful / total) * fit_spiteful
  ;antisocial
  let no_antisocial count agents with [strategy = item 3 strategies]
  set fit_antisocial 0
  if no_antisocial > 0
  [set fit_antisocial mean [fitness] of agents with [strategy = item 3 strategies]]
  let weight_antisocial (no_antisocial / total) * fit_antisocial
  ;altruistic-3P
  let no_altruistic-3P count agents with [strategy = item 4 strategies]
  set fit_altruistic-3P 0
  if no_altruistic-3P > 0
  [set fit_altruistic-3P mean [fitness] of agents with [strategy = item 4 strategies]]
  let weight_altruistic-3P (no_altruistic-3P / total) * fit_altruistic-3P
  ;antisocial-3P
  let no_antisocial-3P count agents with [strategy = item 5 strategies]
  set fit_antisocial-3P 0
  if no_antisocial-3P > 0
  [set fit_antisocial-3P mean [fitness] of agents with [strategy = item 5 strategies]]
  let weight_antisocial-3P (no_antisocial-3P / total) * fit_antisocial-3P
  ;compute new frequencies
  let all_strategies (weight_noone + weight_altruistic + weight_spiteful + weight_antisocial + weight_altruistic-3P + weight_antisocial-3P)
  let new_noone (weight_noone / all_strategies)
  let new_altruistic (weight_altruistic / all_strategies)
  let new_spiteful (weight_spiteful / all_strategies)
  let new_antisocial (weight_antisocial / all_strategies)
  let new_altruistic-3P (weight_altruistic-3P / all_strategies)
  let new_antisocial-3P (weight_antisocial-3P / all_strategies)
  ask agents [die]
  let diff total - ((round(new_noone * total)) + (round(new_altruistic * total)) + (round(new_spiteful * total)) + (round(new_antisocial * total)) + (round(new_altruistic-3P * total)) + (round(new_antisocial-3P * total)))
  create (round(new_noone * total)) (round(new_altruistic * total)) (round(new_spiteful * total)) (round(new_antisocial * total)) (round(new_altruistic-3P * total)) (round(new_antisocial-3P * total))
  ifelse diff > 0
  [repeat diff [create 1 0 0 0 0 0]]
  [ask n-of (- diff) agents [die]]
  mutate
  agent-look
  ask agents [set norm current-norm]
end

to mutate
  ask turtles [
    let r random-float 1
    if r < 0.005 [
      set strategy one-of strategies
      ]
    ]
end

to reset-stats
  set DDs 0
  set CDs 0
  set CCs 0
  set p_fights 0
  set p_dominations 0
  set p_deterred 0
  set g_fights 0
  set g_dominations 0
  set g_deterred 0
end

to go
  update-plots
  reset-stats
  set generation (generation + 1)
  repeat 200 [interact]
  replicate
  tick-advance 1
end
@#$#@#$#@
GRAPHICS-WINDOW
324
48
837
582
16
16
15.242424242424242
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
12
344
108
377
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
107
344
202
377
NIL
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

BUTTON
202
344
297
377
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
12
234
154
267
punishment
punishment
0
100
9
1
1
NIL
HORIZONTAL

TEXTBOX
14
18
258
58
The Coalition Fighting Model
16
0.0
1

SLIDER
155
82
297
115
num_noone
num_noone
0
500
100
1
1
NIL
HORIZONTAL

SLIDER
12
82
154
115
num_spiteful
num_spiteful
0
250
0
1
1
NIL
HORIZONTAL

SLIDER
12
116
154
149
num_altruistic
num_altruistic
0
250
0
1
1
NIL
HORIZONTAL

PLOT
846
129
1151
279
Strategies
Generation
Prevalnece
1.0
10.0
0.0
10.0
true
true
"" ""
PENS
"noone" 1.0 0 -955883 true "" "plotxy generation count agents with [strategy = \"noone\"]"
"altruist" 1.0 0 -6459832 true "" "plotxy generation count agents with [strategy = \"altruistic\"]"
"spite" 1.0 0 -7500403 true "" "plotxy generation count agents with [strategy = \"spiteful\"]"
"antisoc" 1.0 0 -2064490 true "" "plotxy generation count agents with [strategy = \"antisocial\"]"
"altruist-3P" 1.0 2 -6459832 true "" "plotxy generation count agents with [strategy = \"altruistic-3P\"]"
"antisoc-3P" 1.0 2 -2064490 true "" "plotxy generation count agents with [strategy = \"antisocial-3P\"]"

SLIDER
12
200
154
233
pun_cost
pun_cost
0
100
6
1
1
NIL
HORIZONTAL

SLIDER
155
200
297
233
memory
memory
1
100
50
1
1
NIL
HORIZONTAL

SLIDER
12
150
154
183
num_antisocial
num_antisocial
0
250
0
1
1
NIL
HORIZONTAL

PLOT
846
279
1151
429
Cooperation rate
Generation
Proportion
1.0
10.0
0.0
1.0
true
true
"" ""
PENS
"DD" 1.0 0 -13345367 true "" "plotxy generation DDs / (DDs + CCs + CDs + 0.1)"
"CC" 1.0 0 -10899396 true "" "plotxy generation CCs / (DDs + CCs + CDs + 0.1)"
"CD" 1.0 0 -2674135 true "" "plotxy generation CDs / (DDs + CCs + CDs + 0.1)"

PLOT
2
462
162
582
Population size
Generation
agents
0.0
10.0
0.0
10.0
true
false
"" "plot count agents"
PENS
"default" 1.0 0 -16777216 true "" "plotxy generation count agents"

SLIDER
155
268
297
301
sensitivity
sensitivity
0
5
0.8
0.1
1
NIL
HORIZONTAL

SLIDER
12
268
154
301
submit_cost
submit_cost
0
100
6
1
1
NIL
HORIZONTAL

PLOT
846
429
1151
579
Conflict resolution
Generation
Proportion
1.0
10.0
0.0
1.0
true
true
"" ""
PENS
"p_fight" 1.0 0 -2674135 true "" "plotxy generation p_fights / (p_fights + p_dominations + p_deterred + g_fights + g_dominations + g_deterred + 0.1)"
"p_dominate" 1.0 0 -13345367 true "" "plotxy generation p_dominations / (p_fights + p_dominations + p_deterred + g_fights + g_dominations + g_deterred + 0.1)"
"p_deterred" 1.0 0 -10899396 true "" "plotxy generation p_deterred / (p_fights + p_dominations + p_deterred + g_fights + g_dominations + g_deterred + 0.1)"
"g_fights" 1.0 0 -10873583 true "" "plotxy generation g_fights / (p_fights + p_dominations + p_deterred + g_fights + g_dominations + g_deterred + 0.1)"
"g_dominations" 1.0 0 -15390905 true "" "plotxy generation g_dominations / (p_fights + p_dominations + p_deterred + g_fights + g_dominations + g_deterred + 0.1)"
"g_deterred" 1.0 0 -14333415 true "" "plotxy generation g_deterred / (p_fights + p_dominations + p_deterred + g_fights + g_dominations + g_deterred + 0.1)"

SLIDER
12
302
154
335
deterrence
deterrence
0
10
6
0.1
1
NIL
HORIZONTAL

SLIDER
846
96
1018
129
no_generations
no_generations
1
10000
5
1
1
NIL
HORIZONTAL

SLIDER
155
116
296
149
num_altruistic_3P
num_altruistic_3P
0
250
0
1
1
NIL
HORIZONTAL

SLIDER
155
150
296
183
num_antisocial_3P
num_antisocial_3P
0
250
0
1
1
NIL
HORIZONTAL

SLIDER
155
302
297
335
coalition_sensitivity
coalition_sensitivity
0
5
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
155
234
297
267
gossip
gossip
0
100
20
1
1
NIL
HORIZONTAL

CHOOSER
12
386
154
431
altr_rule
altr_rule
"Norm_Only" "Defect_Defectors" "Defect_Defectors&Neutrals"
2

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

cat
false
0
Line -7500403 true 285 240 210 240
Line -7500403 true 195 300 165 255
Line -7500403 true 15 240 90 240
Line -7500403 true 285 285 195 240
Line -7500403 true 105 300 135 255
Line -16777216 false 150 270 150 285
Line -16777216 false 15 75 15 120
Polygon -7500403 true true 300 15 285 30 255 30 225 75 195 60 255 15
Polygon -7500403 true true 285 135 210 135 180 150 180 45 285 90
Polygon -7500403 true true 120 45 120 210 180 210 180 45
Polygon -7500403 true true 180 195 165 300 240 285 255 225 285 195
Polygon -7500403 true true 180 225 195 285 165 300 150 300 150 255 165 225
Polygon -7500403 true true 195 195 195 165 225 150 255 135 285 135 285 195
Polygon -7500403 true true 15 135 90 135 120 150 120 45 15 90
Polygon -7500403 true true 120 195 135 300 60 285 45 225 15 195
Polygon -7500403 true true 120 225 105 285 135 300 150 300 150 255 135 225
Polygon -7500403 true true 105 195 105 165 75 150 45 135 15 135 15 195
Polygon -7500403 true true 285 120 270 90 285 15 300 15
Line -7500403 true 15 285 105 240
Polygon -7500403 true true 15 120 30 90 15 15 0 15
Polygon -7500403 true true 0 15 15 30 45 30 75 75 105 60 45 15
Line -16777216 false 164 262 209 262
Line -16777216 false 223 231 208 261
Line -16777216 false 136 262 91 262
Line -16777216 false 77 231 92 261

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

dog
false
0
Polygon -7500403 true true 300 165 300 195 270 210 183 204 180 240 165 270 165 300 120 300 0 240 45 165 75 90 75 45 105 15 135 45 165 45 180 15 225 15 255 30 225 30 210 60 225 90 225 105
Polygon -16777216 true false 0 240 120 300 165 300 165 285 120 285 10 221
Line -16777216 false 210 60 180 45
Line -16777216 false 90 45 90 90
Line -16777216 false 90 90 105 105
Line -16777216 false 105 105 135 60
Line -16777216 false 90 45 135 60
Line -16777216 false 135 60 135 45
Line -16777216 false 181 203 151 203
Line -16777216 false 150 201 105 171
Circle -16777216 true false 171 88 34
Circle -16777216 false false 261 162 30

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

ghost
false
0
Polygon -7500403 true true 30 165 13 164 -2 149 0 135 -2 119 0 105 15 75 30 75 58 104 43 119 43 134 58 134 73 134 88 104 73 44 78 14 103 -1 193 -1 223 29 208 89 208 119 238 134 253 119 240 105 238 89 240 75 255 60 270 60 283 74 300 90 298 104 298 119 300 135 285 135 285 150 268 164 238 179 208 164 208 194 238 209 253 224 268 239 268 269 238 299 178 299 148 284 103 269 58 284 43 299 58 269 103 254 148 254 193 254 163 239 118 209 88 179 73 179 58 164
Line -16777216 false 189 253 215 253
Circle -16777216 true false 102 30 30
Polygon -16777216 true false 165 105 135 105 120 120 105 105 135 75 165 75 195 105 180 120
Circle -16777216 true false 160 30 30

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

monster
false
0
Polygon -7500403 true true 75 150 90 195 210 195 225 150 255 120 255 45 180 0 120 0 45 45 45 120
Circle -16777216 true false 165 60 60
Circle -16777216 true false 75 60 60
Polygon -7500403 true true 225 150 285 195 285 285 255 300 255 210 180 165
Polygon -7500403 true true 75 150 15 195 15 285 45 300 45 210 120 165
Polygon -7500403 true true 210 210 225 285 195 285 165 165
Polygon -7500403 true true 90 210 75 285 105 285 135 165
Rectangle -7500403 true true 135 165 165 270

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

person business
false
0
Rectangle -1 true false 120 90 180 180
Polygon -13345367 true false 135 90 150 105 135 180 150 195 165 180 150 105 165 90
Polygon -7500403 true true 120 90 105 90 60 195 90 210 116 154 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 183 153 210 210 240 195 195 90 180 90 150 165
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 76 172 91
Line -16777216 false 172 90 161 94
Line -16777216 false 128 90 139 94
Polygon -13345367 true false 195 225 195 300 270 270 270 195
Rectangle -13791810 true false 180 225 195 300
Polygon -14835848 true false 180 226 195 226 270 196 255 196
Polygon -13345367 true false 209 202 209 216 244 202 243 188
Line -16777216 false 180 90 150 165
Line -16777216 false 120 90 150 165

person graduate
false
0
Circle -16777216 false false 39 183 20
Polygon -1 true false 50 203 85 213 118 227 119 207 89 204 52 185
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -8630108 true false 90 19 150 37 210 19 195 4 105 4
Polygon -8630108 true false 120 90 105 90 60 195 90 210 120 165 90 285 105 300 195 300 210 285 180 165 210 210 240 195 195 90
Polygon -1184463 true false 135 90 120 90 150 135 180 90 165 90 150 105
Line -2674135 false 195 90 150 135
Line -2674135 false 105 90 150 135
Polygon -1 true false 135 90 150 105 165 90
Circle -1 true false 104 205 20
Circle -1 true false 41 184 20
Circle -16777216 false false 106 206 18
Line -2674135 false 208 22 208 57

person soldier
false
0
Rectangle -7500403 true true 127 79 172 94
Polygon -10899396 true false 105 90 60 195 90 210 135 105
Polygon -10899396 true false 195 90 240 195 210 210 165 105
Circle -7500403 true true 110 5 80
Polygon -10899396 true false 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -6459832 true false 120 90 105 90 180 195 180 165
Line -6459832 false 109 105 139 105
Line -6459832 false 122 125 151 117
Line -6459832 false 137 143 159 134
Line -6459832 false 158 179 181 158
Line -6459832 false 146 160 169 146
Rectangle -6459832 true false 120 193 180 201
Polygon -6459832 true false 122 4 107 16 102 39 105 53 148 34 192 27 189 17 172 2 145 0
Polygon -16777216 true false 183 90 240 15 247 22 193 90
Rectangle -6459832 true false 114 187 128 208
Rectangle -6459832 true false 177 187 191 208

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

rabbit
false
0
Polygon -7500403 true true 61 150 76 180 91 195 103 214 91 240 76 255 61 270 76 270 106 255 132 209 151 210 181 210 211 240 196 255 181 255 166 247 151 255 166 270 211 270 241 255 240 210 270 225 285 165 256 135 226 105 166 90 91 105
Polygon -7500403 true true 75 164 94 104 70 82 45 89 19 104 4 149 19 164 37 162 59 153
Polygon -7500403 true true 64 98 96 87 138 26 130 15 97 36 54 86
Polygon -7500403 true true 49 89 57 47 78 4 89 20 70 88
Circle -16777216 true false 37 103 16
Line -16777216 false 44 150 104 150
Line -16777216 false 39 158 84 175
Line -16777216 false 29 159 57 195
Polygon -5825686 true false 0 150 15 165 15 150
Polygon -5825686 true false 76 90 97 47 130 32
Line -16777216 false 180 210 165 180
Line -16777216 false 165 180 180 165
Line -16777216 false 180 165 225 165
Line -16777216 false 180 210 210 240

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

spider
true
0
Polygon -7500403 true true 134 255 104 240 96 210 98 196 114 171 134 150 119 135 119 120 134 105 164 105 179 120 179 135 164 150 185 173 199 195 203 210 194 240 164 255
Line -7500403 true 167 109 170 90
Line -7500403 true 170 91 156 88
Line -7500403 true 130 91 144 88
Line -7500403 true 133 109 130 90
Polygon -7500403 true true 167 117 207 102 216 71 227 27 227 72 212 117 167 132
Polygon -7500403 true true 164 210 158 194 195 195 225 210 195 285 240 210 210 180 164 180
Polygon -7500403 true true 136 210 142 194 105 195 75 210 105 285 60 210 90 180 136 180
Polygon -7500403 true true 133 117 93 102 84 71 73 27 73 72 88 117 133 132
Polygon -7500403 true true 163 140 214 129 234 114 255 74 242 126 216 143 164 152
Polygon -7500403 true true 161 183 203 167 239 180 268 239 249 171 202 153 163 162
Polygon -7500403 true true 137 140 86 129 66 114 45 74 58 126 84 143 136 152
Polygon -7500403 true true 139 183 97 167 61 180 32 239 51 171 98 153 137 162

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

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
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="M2run" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>generation = no_generations + 1</exitCondition>
    <metric>generation</metric>
    <metric>count agents with [strategy = item 0 strategies]</metric>
    <metric>count agents with [strategy = item 1 strategies]</metric>
    <metric>count agents with [strategy = item 2 strategies]</metric>
    <metric>count agents with [strategy = item 3 strategies]</metric>
    <metric>count agents with [strategy = item 4 strategies]</metric>
    <metric>count agents with [strategy = item 5 strategies]</metric>
    <metric>DDs</metric>
    <metric>CDs</metric>
    <metric>CCs</metric>
    <metric>p_fights</metric>
    <metric>p_dominations</metric>
    <metric>p_deterred</metric>
    <metric>g_fights</metric>
    <metric>g_dominations</metric>
    <metric>g_deterred</metric>
    <metric>mean [norm] of agents</metric>
    <enumeratedValueSet variable="no_generations">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num_noone">
      <value value="50"/>
      <value value="100"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pun_cost">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="submit_cost">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gossip">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterrence">
      <value value="1"/>
      <value value="3"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sensitivity">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coalition_sensitivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altr_rule">
      <value value="&quot;Norm_Only&quot;"/>
      <value value="&quot;Defect_Defectors&quot;"/>
      <value value="&quot;Defect_Defectors&amp;Neutrals&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>generation = no_generations + 1</exitCondition>
    <metric>generation</metric>
    <metric>count agents with [strategy = item 0 strategies]</metric>
    <metric>count agents with [strategy = item 1 strategies]</metric>
    <metric>count agents with [strategy = item 2 strategies]</metric>
    <metric>count agents with [strategy = item 3 strategies]</metric>
    <metric>count agents with [strategy = item 4 strategies]</metric>
    <metric>count agents with [strategy = item 5 strategies]</metric>
    <metric>DDs</metric>
    <metric>CDs</metric>
    <metric>CCs</metric>
    <metric>p_fights</metric>
    <metric>p_dominations</metric>
    <metric>p_deterred</metric>
    <metric>g_fights</metric>
    <metric>g_dominations</metric>
    <metric>g_deterred</metric>
    <metric>mean [norm] of agents</metric>
    <enumeratedValueSet variable="no_generations">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num_noone">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pun_cost">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="submit_cost">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gossip">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterrence">
      <value value="1"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sensitivity">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coalition_sensitivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altr_rule">
      <value value="&quot;Defect_Defectors&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="M2run2" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>generation = no_generations + 1</exitCondition>
    <metric>generation</metric>
    <metric>count agents with [strategy = item 0 strategies]</metric>
    <metric>count agents with [strategy = item 1 strategies]</metric>
    <metric>count agents with [strategy = item 2 strategies]</metric>
    <metric>count agents with [strategy = item 3 strategies]</metric>
    <metric>count agents with [strategy = item 4 strategies]</metric>
    <metric>count agents with [strategy = item 5 strategies]</metric>
    <metric>DDs</metric>
    <metric>CDs</metric>
    <metric>CCs</metric>
    <metric>p_fights</metric>
    <metric>p_dominations</metric>
    <metric>p_deterred</metric>
    <metric>g_fights</metric>
    <metric>g_dominations</metric>
    <metric>g_deterred</metric>
    <metric>mean [norm] of agents</metric>
    <enumeratedValueSet variable="no_generations">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num_noone">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pun_cost">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="submit_cost">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gossip">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterrence">
      <value value="1"/>
      <value value="3"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sensitivity">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coalition_sensitivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="altr_rule">
      <value value="&quot;Defect_Defectors&quot;"/>
      <value value="&quot;Defect_Defectors&amp;Neutrals&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
