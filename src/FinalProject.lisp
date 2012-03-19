; CS 580 
; Fall 2010

;; Number of pairs to generate
(setf genNumPairs 2000)
;; Structure to store data for each Poker agent in the game
(defstruct PokerAgent 
  agentName      ;; Name of this agent
  currHand       ;; Current hand cards
  currConfidence ;; Current confidence in my hand
  shouldBet      ;; Boolean as to whether it should bet
  numChips       ;; Number of chips left for betting
  numRound       ;; Current round of dealing we are in
  remainingHands ;; Number of hands left to play
)

;; Structore to store data for the Dealer agent
(defstruct DealerAgent
  gameChips      ;; Chips played in the current game
  remainingAgents;; Agents playing in current game
  gameCards      ;; Dealt cards for the game
  gameNumber     ;; The game number
  roundNumber    ;; The round number in this game
  gameWinners    ;; List of agents who won that game
)

(defstruct DebugVars
  gameDeck       ;; Shuffled deck of cards for this game
)

;;betting logic for round one
(defun PokerBet1(player dealer)
  ;;if player has less than 7 chips (meaning last game they can play)
  ;;and confidence is <15%
  ;;and there are more then one games to be play
  ;;then DO NOT BET
  (if (and (> 7 (PokerAgent-numchips player));;
           (and (> 2 (DealerAgent-gameNumber dealer))
                (> 10/100 (PokerAgent-currConfidence player))))
    ;; We don't want to bet this round
    (progn    
      (setf (PokerAgent-shouldBet player) nil)
      ;; Remove this agent from the registered agents with the dealer
      (setf (DealerAgent-remainingAgents dealer) 
		  (remove (PokerAgent-agentName player) 
			  (DealerAgent-remainingAgents dealer))))
    ;; We want to bet so give up one chip to the dealer
    (progn
      (setf (PokerAgent-numChips player) (1- (PokerAgent-numChips player)))
      (setf (DealerAgent-gameChips dealer) (1+ (DealerAgent-gameChips dealer))))
    ))

;;will need to play with the confidence comparisons and gamesNumbers for 2,3,4
;;for round 2---------------------------------------------
(defun PokerBet2 (player dealer)
  ;;if player has <10% confidence 
  ;;OR confidence is <15% and there are more then one games to be play
  ;;then DO NOT BET
  (if (or (> 10/100  (PokerAgent-currConfidence player))
	  (and (> 2 (DealerAgent-gameNumber dealer))
                (> 15/100 (PokerAgent-currConfidence player))))
    ;; We don't want to bet this round
    (progn    
      (setf (PokerAgent-shouldBet player) nil)
      ;; Remove this agent from the registered agents with the dealer
      (setf (DealerAgent-remainingAgents dealer) 
	  (remove (PokerAgent-agentName player) 
		  (DealerAgent-remainingAgents dealer))))

    ;; We want to bet so give up one chip to the dealer
    (progn 
      (setf (PokerAgent-numChips player) (1- (PokerAgent-numChips player)))
      (setf (DealerAgent-gameChips dealer) 
	    (1+ (DealerAgent-gameChips dealer)))))

  ;; If we aren't planning on betting this round see if we generate a 
  ;; random number greater than 179, if we do then place a bet
  ;; This is a "bluff" check
  (if (not (PokerAgent-shouldBet player))
    (setf (PokerAgent-shouldBet player)
	  (< 179 (random 200))))
)

;;for round 3----------------------------------------------
(defun PokerBet3 (player dealer)
  ;;if player has <10% confidence 
  ;;OR confidence is <20% and there are more then one games to be play
  ;;then DO NOT BET
  (if (or (> 10/100  (PokerAgent-currConfidence player))
	  (and (> 2 (DealerAgent-gameNumber dealer))
                (> 20/100 (PokerAgent-currConfidence player))))
    ;; We don't want to bet this round
    (progn    
      (setf (PokerAgent-shouldBet player) nil)
      ;; Remove this agent from the registered agents with the dealer
      (setf (DealerAgent-remainingAgents dealer) 
		  (remove (PokerAgent-agentName player) 
			  (DealerAgent-remainingAgents dealer))))

    ;; We want to bet so give up one chip to the dealer
    (progn 
      (setf (PokerAgent-numChips player) (1- (PokerAgent-numChips player)))
      (setf (DealerAgent-gameChips dealer) 
	    (1+ (DealerAgent-gameChips dealer)))))

  ;; If we aren't planning on betting this round see if we generate a 
  ;; random number greater than 181, if we do then place a bet
  ;; This is a "bluff" check
  (if (not (PokerAgent-shouldBet player))
    (setf (PokerAgent-shouldBet player)
	  (< 181 (random 200))))
)

;;for round 4----------------------------------------------
(defun PokerBet4 (player dealer)
  ;;if player has <15% confidence 
  ;;OR confidence is <25% and there are more then one games to be play
  ;;then DO NOT BET
  (if (or (> 15/100  (PokerAgent-currConfidence player))
	  (and (> 2 (DealerAgent-gameNumber dealer))
                (> 25/100 (PokerAgent-currConfidence player))))
    ;; We don't want to bet this round
    (progn    
      (setf (PokerAgent-shouldBet player) nil)
      ;; Remove this agent from the registered agents with the dealer
      (setf (DealerAgent-remainingAgents dealer) 
		  (remove (PokerAgent-agentName player) 
			  (DealerAgent-remainingAgents dealer))))

    ;; We want to bet so give up one chip to the dealer
    (progn 
      (setf (PokerAgent-numChips player) (1- (PokerAgent-numChips player)))
      (setf (DealerAgent-gameChips dealer) 
	    (1+ (DealerAgent-gameChips dealer)))))
  ;; If we aren't planning on betting this round see if we generate a 
  ;; random number greater than 181, if we do then place a bet
  ;; This is a "bluff" check
    (if (not (PokerAgent-shouldBet player))
    (setf (PokerAgent-shouldBet player)
	  (< 189 (random 200))))
)

;; Main game function
(defun PokerMaster (numAgents numChips numGames)
  (let ((communalCards) (pokerAgents (list)))
     (setf gameDealer (make-DealerAgent))

     ;; Setup the pokerAgents
     (loop for i from 0 to (- numAgents 1) do
       ;; Create a new pokerAgent
       (setf pokerAgents (append pokerAgents 
				 (list (make-PokerAgent))))
       ;; Set the new agent's name to the value of i
       (setf (PokerAgent-agentName (nth i pokerAgents)) 
  			  i)
       ;; Set the number of games remaining
       (setf (PokerAgent-remainingHands (nth i pokerAgents)) 
			numGames)
       ;; Hand out the chips to this agent
       (setf (PokerAgent-numChips (nth i pokerAgents)) 
			numChips)
     )
     
     

     ;; Loop to play each game
     (loop for gameNum from 1 to numGames do
       (format t "~%------- Game number ~S -------~%" 
	      gameNum)

      ;; Initialize the dealer agent
      (Dealer gameDealer numAgents)
      (setf (DealerAgent-remainingAgents gameDealer) NIL)
      (setf (DealerAgent-gameNumber gameDealer) gameNum)
      
      ;; Setup the agents with their cards and reset fields for the
      ;; new game
      (loop for i from 0 to (- numAgents 1) do
        (setf (DealerAgent-remainingAgents gameDealer) 
	      (append (DealerAgent-remainingAgents gameDealer) 
	  	      (list (PokerAgent-agentName (nth i pokerAgents)))))
        (setf (PokerAgent-currHand (nth i pokerAgents)) 
		  	   (nth i (DealerAgent-gameCards gameDealer)))
        (setf (PokerAgent-currConfidence (nth i pokerAgents)) 
			0)
        (setf (PokerAgent-numRound (nth i pokerAgents)) 
			0)
        (setf (PokerAgent-shouldBet (nth i pokerAgents)) 
			T)
      )

      ;; Grab the communal cards from the dealer, makes things easier 
      ;; when getting the cards for each round
      (setf communalCards (first (last (DealerAgent-gameCards gameDealer))))
			    
      (format t "~%Evaluating dealt cards~%")
      ;; Loop through each agent and calculate their confidence in their hand
      ;; and place their bet
      (loop for agent in pokerAgents do
            (if (subsetp (list (PokerAgent-agentName agent))
                         (DealerAgent-remainingAgents gameDealer))
              (progn
	      ;; Set this agents confidence in their hand
                (setf (PokerAgent-currConfidence agent)
                        (HandRank2 (PokerAgent-currHand agent) sorted1000))
		  ;; Place our bet
                (PokerBet1 agent gameDealer))
              )
            (setf (PokerAgent-numRound agent) 1)
      )
      
      ;; Print out poker agent data
      (PrintHand pokerAgents)

      ;; Check to see if there are more than one agent left to play
      (if (> (length (DealerAgent-remainingAgents gameDealer)) 1)
	(progn 
         ;; Loop through each agent and calculate their confidence in their hand
         ;; and place their bet
	  (format t "~%Evaluating dealt cards plus three communal cards ~S~%"
	     (list (first communalCards) (second communalCards)
		       (third communalCards)))
     	(loop for agent in pokerAgents do
          (if (subsetp (list (PokerAgent-agentName agent))
		   (DealerAgent-remainingAgents gameDealer))
            (progn
	      ;; Set this agents confidence in their hand
              (setf (PokerAgent-currConfidence agent)
  	        (HandRank5 (PokerAgent-currHand agent) 
		 (list (first communalCards) (second communalCards)
		       (third communalCards)))) 

	      ;; Place our bet
              (PokerBet2 agent gameDealer))
            (setf (PokerAgent-numRound agent) 2)
        ))
      
      
	;; Print out poker agent data
        (PrintHand pokerAgents)))

      ;; Check to see if there are more than one agent left to play
      (if (> (length (DealerAgent-remainingAgents gameDealer)) 1)
	(progn
         ;; Loop through each agent and calculate their confidence in their hand
         ;; and place their bet
          (format t "~%Evaluating dealt cards plus four communal cards ~S~%"
	     (list (first communalCards) (second communalCards)
		       (third communalCards)(fourth communalCards)))
          (loop for agent in pokerAgents do
            (if (subsetp (list (PokerAgent-agentName agent))
		   (DealerAgent-remainingAgents gameDealer))
             (progn
	      ;; Set this agents confidence in their hand
	       (setf (PokerAgent-currConfidence agent)
	         (HandRank6 (PokerAgent-currHand agent) 
		   (list (first communalCards) (second communalCards)
		         (third communalCards)(fourth communalCards))))
	      ;; Place our bet
              (PokerBet3 agent gameDealer))
            (setf (PokerAgent-numRound agent) 3)
          ))
      
          ;; Print out poker agent data
          (PrintHand pokerAgents)))
   
      ;; Check to see if there are more than one agent left to play
      (if (> (length (DealerAgent-remainingAgents gameDealer)) 1)
	(progn 
         ;; Loop through each agent and calculate their confidence in their hand
         ;; and place their bet
          (format t "~%Evaluating dealt cards plus five communal cards ~S~%"
	     (list (first communalCards) (second communalCards)
		   (third communalCards)(fourth communalCards)
	     (fifth communalCards)))
          (loop for agent in pokerAgents do
            (if (subsetp (list (PokerAgent-agentName agent))
		   (DealerAgent-remainingAgents gameDealer))
            (progn
	      ;; Set this agents confidence in their hand
              (setf (PokerAgent-currConfidence agent)
 	        (HandRank7 (PokerAgent-currHand agent) 
		 (list (first communalCards) (second communalCards)
		       (third communalCards)(fourth communalCards)
		       (fifth communalCards))))
	      ;; Place our bet
              (PokerBet4 agent gameDealer))
          
             (setf (PokerAgent-numRound agent) 4)
          ))
  
        ;; Print out poker agent data
        (PrintHand pokerAgents)))

      ;; Determine who won this game and print out the winner
      (DetermineWinner gameDealer pokerAgents)
      (format t "~%This game's winner(s):~%")
      ;; Loop through the winners of the hand printing out their hand
      ;; and splitting the pot between them
      (loop for winner in (DealerAgent-gameWinners gameDealer) do
	(loop for agent in pokerAgents do
	  (if (eq winner (PokerAgent-agentName agent))
            (progn
              (format t "Agent ~S with hand ~S~%"
		      winner
	              (PokerAgent-currHand agent))
	      ;; Split the pot between the number of winners, any extra chips
	      ;; get thrown away
              (setf (PokerAgent-numChips agent)
	            (+ (floor (/ (DealerAgent-gameChips gameDealer) 
		       (length (DealerAgent-gameWinners gameDealer))))
		   (PokerAgent-numChips agent))))
	    )))
      
      ;; Update the number of games that remain to play
      (loop for agent in pokerAgents do
	    (setf (PokerAgent-remainingHands agent) (- numGames gameNum)))
      (format t "~%")

      ;; remove any agents that don't have enough chips to play
      (setf pokerAgents (RemoveAgents pokerAgents))
      (setf numAgents (length pokerAgents))
      
      ;; Print the game results
      (PrintGame pokerAgents)
      (print '-----------------------------)

      ;; If there is only one agent with chips then this game is over
      (if (eq numAgents 1)
	(setf gameNum numGames))
      
      ;; Pause between the games
      (sleep 4)
      
      ;;once the game is over, display order of finish and winnings
      (if (= numGames (DealerAgent-gameNumber gameDealer))
          (progn
            (format t "~%~%GAME-OVER~%...and the order of finish is:~%")
            (format t "~%+++++++++++++++++++++++++++++++++~%")
            (setf winList (sort pokerAgents #'(lambda (x y)
                                                (> (PokerAgent-numChips x)
                                                   (PokerAgent-numChips y)))))
            (dotimes (place (length winList))
              (format t "~S. - Agent: ~S with ~S chips total~%"
                      (1+ place)
                      (PokerAgent-agentName (nth place winList))
                      (PokerAgent-numChips (nth place winList))))
            (format t "+++++++++++++++++++++++++++++++++")
            (print '___)
            )
        )
    )
  )
)

;; Update end of game players
(defun RemoveAgents (pokerAgents)
  (let ((remainingAgents(list)))
  (loop for agent in pokerAgents do
    ;; If the agent has more than 3 chips add them to the remaining agents list
    (if (> (PokerAgent-numChips agent) 3)
      (setf remainingAgents (append remainingAgents (list agent)))
      ;; Otherwise print out the agents no longer playing
      (format t "Agent ~S no longer playing because of a lack of chips~%"
        (PokerAgent-agentName agent)))) remainingAgents)
)

;; Print the agentname, their hand, the confidence they have in their hand,
;; and whether they are betting or not
(defun PrintHand (pokerAgents)
  (loop for agent in pokerAgents do
        (if (PokerAgent-shouldBet agent)
          (format t 
	   "Agent: ~S has the hand ~S with confidence ~S and is betting~%"
	    (PokerAgent-agentName agent)
	    (PokerAgent-currHand agent)
	    (float (PokerAgent-currConfidence agent)))
          (format t 
	   "Agent: ~S has the hand ~S with confidence ~S and is not betting~%"
	    (PokerAgent-agentName agent)
	    (PokerAgent-currHand agent)
	    (float (PokerAgent-currConfidence agent))))
    )
)

;; Print the agentname and their remaining chips
(defun PrintGame (pokerAgents)
  (format t "~%")
  (loop for agent in pokerAgents do
        ;(if (PokerAgent-shouldBet agent)
    (format t "Agent: ~S has ~S chips remaining~%"
	    (PokerAgent-agentName agent)
	    (PokerAgent-numChips agent)))
    ;)
)

;; Initialize the dealerAgent 
(defun Dealer (dealerAgent numAgents)
    (setf (DealerAgent-gameChips gameDealer) 0)
    ;; Deal out the number of cards needed for numAgents
    (setf (DealerAgent-gameCards gameDealer) 
	  (DealCards (RandomShuffle cards) numAgents))
    (setf (DealerAgent-remainingAgents gameDealer) (list))
)

;; Loop through the remaining poker agents and find the winner
(defun DetermineWinner (gameDealer pokerAgents)
  (let ((topHand)(topAgents)(compareVal))
    (format t "~%The best hand for the Agents are:~%")

    (loop for agent in pokerAgents do
      ;; If this agent is in the list of remaining agents check its cards
      ;; otherwise skip it
      (if (subsetp  (list (PokerAgent-agentName agent)) 
		  (DealerAgent-remainingAgents gameDealer))
        (progn
	  ;; Set the agents current hand to the best hand they could have 
	  ;; given their dealt hand and the communal cards
	  (setf (PokerAgent-currHand agent)
	    (GetBestHand (append (PokerAgent-currHand agent)
			 (first (last (DealerAgent-gameCards gameDealer))))))
	  ;; Print out the agent and their best hand
	  (format t "Player ~S hand ~S~%"
			  (PokerAgent-agentName agent) 
			  (PokerAgent-currHand agent))
	  ;; If this is the first loop then this agent has the best hand
          (if (eq topHand nil)
	    (progn
	      (setf topHand (PokerAgent-currHand agent))
	      (setf topAgents (list (PokerAgent-agentName agent)))
            )
	    ;; Otherwise compare this agent's hand to the best hand out there
	    (progn
	       (setf compareVal (CompareHands (PokerAgent-currHand agent)
			    topHand))
	       ;; If this agent has the best hand then store it
               (if (eq compareVal 1)
                (progn 
  	          (setf topHand (PokerAgent-currHand agent))
 	          (setf topAgents (list (PokerAgent-agentName agent)))
	        )
		;; If this agent is tied with the best hand then add this 
		;; agent's name to the list of top agents
	        (if (eq compareVal 0)
	          (setf topAgents (append topAgents 
				  (list (PokerAgent-agentName agent))))))
	    ))
	)
      )
    )
    ;; Store the list of winning agents in the gameDealer 
    (setf (DealerAgent-gameWinners gameDealer) topAgents)
  )
)

;;=========================================================
;;=========Code below is from earlier assignments==========
;;=========================================================

;; Deal out the cards for the deck
(defun DealCards (cardDeck numPlayers)
  (let ((dealtCards))
    (setf dealtCards (list ))
    (setf numPlayers (- numPlayers 1))
    (loop for i from 0 to numPlayers do                     ;; Deal player cards
	  (progn
	    (setf dealtCards (append dealtCards 
				     (list (list (nth i cardDeck)
					   (nth (+ i numPlayers 1) cardDeck)))))
	  )
    )
    (loop for i from 0 to numPlayers do        ;; Remove the cards already dealt
	(setf cardDeck (cdr (cdr cardDeck)))  ;; Remove 1 set of cards from deck
    )

    (setf dealtCards (append dealtCards           ;; Create the list of discards
			     (list (list (first cardDeck)
				   (fifth cardDeck)
				   (seventh cardDeck)))))
    (setf dealtCards (append dealtCards             ;; Create the community pile
			     (list (list (second cardDeck)
	 	 	     (third cardDeck)
			     (fourth cardDeck)
		             (sixth cardDeck)
	 		     (eighth cardDeck)))))
  )
)



;; HandRank2 takes in two lists, first list is the pair of dealt cards for 
;; player one, the other is a list of pairs.  The return value is 
;; the rank of this hand to over a thousand other pairs.
(defun HandRank2 (cardPair pairsList)
  (let ((comparePair)(retVal 0))
    ;; Loop through the pairs list and see where this pair occurs
    ;; When the pair is found set the return value the index divided by
    ;; the length of the pairs list
   (loop for i from (length pairsList) downto 1 do
    (setf comparePair (nth (- i 1) pairsList))
     (if (equal (first cardPair)(first comparePair))
       (if (equal (second cardPair)(second comparePair))
	 (setf retVal (/ i (length pairsList))))
       (if (equal (first cardPair) (second comparePair))
	 (if (equal (second cardPair)(first comparePair))
	   ;(setf retVal  i))))
	   (setf retVal (/ i (length pairsList))))))
   )
   retVal
  )
)

;; HandRank5 takes in two lists, first list is the pair of dealt cards for 
;; player one, the other is a list of 3 communal cards.  The return value is 
;; the number of hands the player can beat out of 2000 randomly chosen opponent
;; cards.
(defun HandRank5 (handCards communalCards)
 (let ((retVal 0)(opponentCards))
   (setf handCards (append handCards communalCards))
   (setf opponentCards (RandomPairs genNumPairs 
			handCards)) ;; Set opponentCards 
   ;; equal to 2000 randomly select pairs

   ;; Loop through all opponent hands and determine how many hands this player
   ;; could beat
   (loop for opponentHand in opponentCards do
    (setf opponentHand (append opponentHand communalCards))
    (if (eq 1 (compareHands handCards opponentHand))
      (setf retVal (+ 1 retVal)))
   )
   ;; Return the number of cards the players hand is better than
   (/ retVal genNumPairs)
 )
)


;; HandRank6 takes in two lists.  First list are the cards already dealt
;; to the player.  The second list contains all the cards dealt into the
;; communal pot.  The return value is the number of hands the player can
;; beat out of 2000 randomly chosen opponent hands
(defun HandRank6 (handCards communalCards)
 (let ((retVal 0)(opponentCards)(myBest)(theirBest)(testCards))
   (setf handCards (append handCards communalCards)) ;; Combine players dealt
   ;; Cards with the communal cards
   (setf opponentCards (RandomPairs genNumPairs handCards)) ;; Set opponentCards 
   ;; equal to 2000 randomly select pairs
   (setf myBest (delete-nth handCards 0)) ;; myBest is the players best hand
   ;; First set the hand equal to the communal cards and their second dealt 
   ;; card
   
   ;; Loop through the 6 possible cards eliminating one card from the hand each
   ;; time and compare the new hand to the best hand thus far.  This is for
   ;; the known players hand
   (loop for i from 1 to (- (length handCards) 1) do
    (if (> (compareHands (delete-nth handCards i) myBest) 0)
      (setf myBest (delete-nth handCards i)))
   )
   ;; Loop through all generated hands and find the the best cards for each 
   ;; generated pair
   (loop for opponentHand in opponentCards do
    (setf opponentHand (append opponentHand communalCards))
    (setf theirBest (rest opponentHand ))
   ;; Loop through the 6 possible cards eliminating one card from the hand each
   ;; time and compare the new hand to the best hand thus far.  This is for
   ;; the randomly generated hand hand
    (loop for i from 1 to (- (length opponentHand) 1) do
     (setf testCards (delete-nth opponentHand i))
     (if (> (compareHands testCards theirBest) 0)
      (setf theirBest testCards))
    )
    ;; If the known players cards are better than the best potential had from
    ;; the randomly generated cards add 1
    (if (eq 1 (compareHands myBest theirBest))
      (setf retVal (+ 1 retVal)))
   )
   ;; Return the number of cards the players hand is better than
   (/ retVal genNumPairs)
 )
)

;; HandRank7 takes in two lists.  First list are the cards already dealt
;; to the player.  The second list contains all the cards dealt into the
;; communal pot.  The return value is the number of hands the player can
;; beat out of 2000 randomly chosen opponent hands
(defun HandRank7 (handCards communalCards)
 (let ((retVal 0)(opponentCards)(myBest)(theirBest)(testcards))
   (setf handCards (append handCards communalCards))
   (setf opponentCards (RandomPairs genNumPairs handCards)) ;; Set opponentCards 
   ;; equal to 2000 randomly select pairs
   (setf myBest (delete-nth handCards 0));; Set myBest hand to the 5 communal
   (setf myBest (delete-nth myBest 0)) ;; cards
   ;; Loop through the dealt cards and communal cards removing two different
   ;; cards from the hand during each loop. There are six possible combinations
   ;; for the first outter loop with hands and then 5 and then 4...
   ;; This gives us 6+5+4+3+2+1 which is 21 different hands with the 7 cards
   (loop for i from 1 to (- (length handCards) 2) do
    (loop for j from i to (- (length handCards) 1) do
      ;; Compare the best hand found thus far to the next possible hand
      ;; and store the best hand of the two for later comparison
     (if (> (compareHands (delete-nth (delete-nth handCards j) i) myBest) 0)
      (setf myBest  (delete-nth (delete-nth handCards j) i)))
    )
   )

   ;; Loop through all opponent hand combinations and compare the best hand
   ;; of each opponent pair to the known players best hand
   (loop for opponentHand in opponentCards do
    (setf opponentHand (append opponentHand communalCards))
    (setf theirBest (cdr opponentHand ))
    (setf theirBest (cdr theirBest))

    (loop for i from 1 to (- (length opponentHand) 2) do
     (loop for j from i to (- (length opponentHand) 1) do
       (setf testCards (delete-nth (delete-nth opponentHand j) i))
      (if (> (compareHands testCards theirBest) 0)
       (setf theirBest testCards))
     )
    )
    ;; If the known player has the better hand increment retVal by one
    (if (eq 1 (compareHands myBest theirBest))
      (setf retVal (+ 1 retVal)))
   )
   (/ retVal genNumPairs)
 )
)

(defun getBestHand (agentCards)
  (let ((myBest))
   (setf myBest (delete-nth agentCards 0));; Set myBest hand to the 5 communal
   (setf myBest (delete-nth myBest 0)) ;; cards
   ;; Loop through the dealt cards and communal cards removing two different
   ;; cards from the hand during each loop. There are six possible combinations
   ;; for the first outter loop with hands and then 5 and then 4...
   ;; This gives us 6+5+4+3+2+1 which is 21 different hands with the 7 cards
   (loop for i from 1 to (- (length agentCards) 2) do
    (loop for j from i to (- (length agentCards) 1) do
      ;; Compare the best hand found thus far to the next possible hand
      ;; and store the best hand of the two for later comparison
     (if (> (compareHands (delete-nth (delete-nth agentCards j) i) myBest) 0)
      (setf myBest  (delete-nth (delete-nth agentCards j) i)))
    )
   )
   myBest
  )
)
;; Delete nth element in an array
(defun delete-nth (cards  n)
  (remove-if #' (lambda (x)     ;; Remove the card at index n
      (equal (nth n cards) x)) cards)
)

;; Setup cards to be equal to a complete deck
(setf cards '((A D)(2 D)(3 D)(4 D)(5 D)(6 D)(7 D)(8 D)(9 D)(10 D)(J D)(Q D)(K D)
             (A C)(2 C)(3 C)(4 C)(5 C)(6 C)(7 C)(8 C)(9 C)(10 C)(J C)(Q C)(K C)
             (A S)(2 S)(3 S)(4 S)(5 S)(6 S)(7 S)(8 S)(9 S)(10 S)(J S)(Q S)(K S)
             (A H)(2 H)(3 H)(4 H)(5 H)(6 H)(7 H)(8 H)(9 H)(10 H)(J H)(Q H)(K H)))

;; RandomPairs generates numPairs number of pairs minus the usedCards
(defun RandomPairs (numPairs usedCards)
 (let ((retVal (list))(usableCards)(remainingCards)(num)(tempPair (list)))
  (setf usableCards cards)
  ;; Loop through base cards and remove cards already dealt
  (loop for card in usedCards do
   (setf usableCards (remove-if #' (lambda (x)
	(equal card x)) usableCards))
  )
  ;; Loop until we have numPairs number of random pairs
  (loop while (< (length retVal) numPairs) do
    ;; If we have only one card left in the deck then set the deck to a 
    ;; new random deck
   (if (< (length remainingCards) 2)
     (setf remainingCards (RandomShuffle usableCards)))
   ;; Get a random index to grab a card from the deck and append it to
   ;; tempPair
   (setf num (random (length remainingCards)))
   (setf tempPair (append tempPair (list (nth num remainingCards))))
   ;; Remove the card just grabbed
   (setf remainingCards (delete-nth remainingCards num))
   
   ;; If we have two cards and and empty list then add this set to the retVal
   (if (eq (length tempPair) 2)
     (if (equal (nth 0 tempPair)(nth 1 tempPair))
       (setf tempPair (list))
       (progn
         (setf retVal (append retVal (list  tempPair)))
         (setf tempPair (list))
       )
     )
   )
  )
  retVal
 )
)

;; Takes in a deck and shuffles it
(defun RandomShuffle (cardDeck)
  (let ((shuffledDeck) (num 0))
    (setf shuffledDeck (list ()))   ;; Initialize variable to be a list with nil
    (loop for i from (length cardDeck) downto 1 do    ;; Loop the same number of
     (progn                              ;; times as there are cards in the deck
      (setf num (length cardDeck))                ;; get the remaining deck size
      (setf num (random num))    ;; pick a random number between 0 and deck size
      (setf shuffledDeck (append shuffledDeck (list (nth num cardDeck)))) 
                                                                     ;; Add that
                                                 ;; element to the shuffled deck
      (setf cardDeck (remove-if #' (lambda (x)     ;; remove the last card added
				                      ;; from the remaining deck
				    (equal (nth num cardDeck) x)) cardDeck))
     )
    )
    (cdr shuffledDeck)     ;; Remove the first element since it is a nil element
  )
)



;; replace A by 1, J by 11, Q by 12, K by 13
(defun NumericalHand (hand)
  (let ((new-hand nil) (cardvals nil) (straight nil))
    (setf new-hand
	  (mapcar #'(lambda (x) 
		      (cond ((eq (first x) 'A) (list 1 (second x)))
			    ((eq (first x) 'J) (list 11 (second x)))
			    ((eq (first x) 'Q) (list 12 (second x)))
			    ((eq (first x) 'K) (list 13 (second x)))
			    (t x)))
		  hand))
    (setf cardvals (sort (mapcar #'(lambda (x) (first x)) new-hand) #'<))
    (setf straight (and (eq (first cardvals) 1) (eq (second cardvals) 2) (eq (third cardvals) 3)
			(eq (fourth cardvals) 4) (eq (fifth cardvals) 5)))
    (if straight (SortHand new-hand)   ;; keep A's as 1's - have straight
	(SortHand (mapcar #'(lambda (x) (if (eq (first x) 1) (list 14 (second x)) x)) new-hand)))))
	
;; Sort hand by card value
(defun SortHand (hand)
  (sort hand #'(lambda (x y) (< (first x) (first y)))))

;; check if all cards are different
(defun All-Diff (cardvals)
  (and (< (first cardvals) (second cardvals)) (< (second cardvals) (third cardvals))
       (< (third cardvals) (fourth cardvals)) (< (fourth cardvals) (fifth cardvals))))

;; Evaluate hand from strongest to weakest
;; Input: 5 card hand
;; Return value:
;;   ((RoyalFlush Suit) ;; have royal flush give suit, otherwise nil
;;    (StraightFlush HighCard Suit)  ;; high card value & suit, otherwise nil
;;    (FourOfAKind CardVal)
;;    (FullHouse HighCardVal LowCardVal)   ;; HighCardVal - three of a kind value, LowCard - pair value
;;    (Flush HighCard Suit)
;;    (Straight HighCard)
;;    (ThreeOfAKind CardVal)
;;    (TwoPair HighCardVal LowCardVal)
;;    (Pair CardVal)
;;    HighCardVal)
(defun EvaluateHand (hand)
  (let ((cardvals (mapcar #'(lambda (x) (first x)) hand)) 
	(suits (mapcar #'(lambda (x) (second x)) hand))
	(suits-equal nil)
	(ret-val (list nil nil nil nil nil nil nil nil nil 
		       (if (eq (first (first hand)) 1) 14 (first (fifth hand))))))
    (setf suits-equal (every #'(lambda (x) (eq x (first suits))) (rest suits)))
    ;; check for Royal Flush
    (if (and suits-equal (eq (first cardvals) 10) (eq (fifth cardvals) 14))
	(setf (first ret-val) (list 'RoyalFlush (first suits))))
    (if (first ret-val) (return-from EvaluateHand ret-val))

    ;; check for Straight Flush
    (if (and suits-equal (eq (fifth cardvals) (+ (first cardvals) 4)))
	(setf (second ret-val) (list 'StraightFlush (fifth cardvals) (first suits))))
    (if (second ret-val) (return-from EvaluateHand ret-val))

    ;; check for Four of a Kind
    (cond ((eq (first cardvals) (fourth cardvals)) 
	   (setf (third ret-val) (list 'FourOfAKind (first cardvals))))
	  ((eq (second cardvals) (fifth cardvals)) 
	   (setf (third ret-val) (list 'FourOfAKind (second cardvals))))
	  (t nil))
    (if (third ret-val) (return-from EvaluateHand ret-val))

    ;; check for Full House
    (cond ((and (eq (first cardvals) (third cardvals))
		(eq (fourth cardvals) (fifth cardvals)))
	   (setf (fourth ret-val) (list 'FullHouse (first cardvals) (fourth cardvals))))
	  ((and (eq (first cardvals) (second cardvals))
		(eq (third cardvals) (fifth cardvals)))
	   (setf (fourth ret-val) (list 'FullHouse (third cardvals) (first cardvals))))
	  (t nil))
    (if (fourth ret-val) (return-from EvaluateHand ret-val))

    ;; check for Flush
    (if suits-equal (setf (fifth ret-val) (list 'Flush (fifth cardvals) (first suits))))
    (if (fifth ret-val) (return-from EvaluateHand ret-val))

    ;; check for Straight;;    CHECK THIS CAREFULLY
    (if (and (all-diff cardvals) (eq (fifth cardvals) (+ (first cardvals) 4)))
	(setf (sixth ret-val) (list 'Straight (fifth cardvals))))
    (if (sixth ret-val) (return-from EvaluateHand ret-val))

    ;; check for Three of a Kind
    (if (or (eq (first cardvals) (third cardvals))
	    (eq (second cardvals) (fourth cardvals))
	    (eq (third cardvals) (fifth cardvals)))
	(setf (seventh ret-val) (list 'ThreeOfAKind (third cardvals))))
    (if (seventh ret-val) (return-from EvaluateHand ret-val))

    ;; check for Two Pair
    (if (or (and (eq (first cardvals) (second cardvals))
		 (or (eq (third cardvals) (fourth cardvals))
		     (eq (fourth cardvals) (fifth cardvals))))
	    (and (eq (second cardvals) (third cardvals))
		 (eq (fourth cardvals) (fifth cardvals))))
	(setf (eighth ret-val) (list 'TwoPair (fourth cardvals) (second cardvals))))
    (if (eighth ret-val) (return-from EvaluateHand ret-val))

    ;; check for a Pair
    (cond ((eq (first cardvals) (second cardvals)) 
	   (setf (ninth ret-val) (list 'Pair (first cardvals))))
	  ((eq (second cardvals) (third cardvals)) 
	   (setf (ninth ret-val) (list 'Pair (second cardvals))))
	  ((eq (third cardvals) (fourth cardvals)) 
	   (setf (ninth ret-val) (list 'Pair (third cardvals))))
	  ((eq (fourth cardvals) (fifth cardvals)) 
	   (setf (ninth ret-val) (list 'Pair (fourth cardvals))))
	  (t nil))
    (if (ninth ret-val) (return-from EvaluateHand ret-val))
    ret-val
    ))


;; function to break ties; compare two hands until one card is higher or done
;; return 1 if hand1 is better, -1 if hand2 is better and 0 otherwise
(defun BreakTies (cvals1 cvals2)
  ;; (format t "~% cvals1 = ~A    cvals2 = ~A ~%" cvals1 cvals2)  
  (cond ((or (null cvals1) (null cvals2)) 0)
	((> (first cvals1) (first cvals2)) 1)
	((< (first cvals1) (first cvals2)) -1)
	(t (BreakTies (rest cvals1) (rest cvals2))))
)

;; given two hands of five cards each, determine if hand1 is stronger than hand2 
;; there are three possible return values
;; 1: hand1 is better
;; -1: hand2 is better
;; 0: hands are equal
(defun CompareHands (hand1 hand2)
  (let ((nhand1 (NumericalHand hand1))
	(nhand2 (NumericalHand hand2)) (hs1 nil) (hs2 nil) (cvals1 nil) (cvals2))
    (setf hs1 (EvaluateHand nhand1))
    (setf hs2 (EvaluateHand nhand2))
    (setf cvals1 (reverse (mapcar #'(lambda(x) (first x)) nhand1)))
    (setf cvals2 (reverse (mapcar #'(lambda(x) (first x)) nhand2)))

    ;; (format t "~% nhand1 = ~A   cvals1 = ~A    hs1 = ~A" nhand1 cvals1 hs1)
    ;; (format t "~% nhand2 = ~A   cvals2 = ~A    hs2 = ~A" nhand2 cvals2 hs2)
    (cond ((or (first hs1) (first hs2))
	   ;; one player has a RoyalFlush
	   (cond ((null (first hs1)) -1)  ;; hand2 is better
		 ((null (first hs2))  1)  ;; hand1 is better
		 (t 0)))
	  ;; one player has a StraightFlush
	  ((or (second hs1) (second hs2))
	   (cond ((null (second hs1)) -1)   ;; player2 has SF
		 ((null (second hs2))  1)   ;; player1 has SF
		 ((> (second (second hs1)) (second (second hs2))) 1)  ;; higher card
		 ((< (second (second hs1)) (second (second hs2))) -1) ;; higher card
		 (t 0)))  ;; hands equal
	   ;; one player has Four of a Kind
	  ((or (third hs1) (third hs2))
	   (cond ((null (third hs1)) -1)   ;; player2 has FofaK
		 ((null (third hs2))  1)   ;; player1 has FofaK
		 ((> (second (third hs1)) (second (third hs2))) 1)  ;; higher card
		 ((< (second (third hs1)) (second (third hs2))) -1) ;; higher card
		 (t (BreakTies cvals1 cvals2)))) ;; break ties by finding the highest card that differs 
;;		 ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
;;		 ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		 ;; (t 0)))   ;; hands equal
	  ;; one player has Full House
	  ((or (fourth hs1) (fourth hs2))
	   (cond ((null (fourth hs1)) -1)   ;; player2 has FH
		 ((null (fourth hs2))  1)   ;; player1 has FH
		 ((> (second (fourth hs1)) (second (fourth hs2))) 1)  ;; higher card
		 ((< (second (fourth hs1)) (second (fourth hs2))) -1) ;; higher card
		 (t 0)))   ;; hands equal
	  ;; one player has Flush
	  ((or (fifth hs1) (fifth hs2))
	   (cond ((null (fifth hs1)) -1)   ;; player2 has F
		 ((null (fifth hs2))  1)   ;; player1 has F
		 (t (BreakTies cvals1 cvals2))))   ;; break ties by finding the highest card that differs
		 ;; ((> (second (fifth hs1)) (second (fifth hs2))) 1)  ;; higher card
		 ;; ((< (second (fifth hs1)) (second (fifth hs2))) -1) ;; higher card
		 ;; (t 0)))   ;; hands equal
	  ;; one player has Straight
	  ((or (sixth hs1) (sixth hs2))
	   (cond ((null (sixth hs1)) -1)   ;; player2 has S
		 ((null (sixth hs2))  1)   ;; player1 has S
		 ((> (second (sixth hs1)) (second (sixth hs2))) 1)  ;; higher card
		 ((< (second (sixth hs1)) (second (sixth hs2))) -1) ;; higher card
		 (t 0)))   ;; hands equal
	  ;; one player has Three of a Kind
	  ((or (seventh hs1) (seventh hs2))
	   ;; (format t "~% Three of a Kind!!!! ~A ~A~%" (seventh hs1) (seventh hs2))
	   (cond ((null (seventh hs1)) -1)   ;; player2 has TofaK
		 ((null (seventh hs2))  1)   ;; player1 has TofaK
		 ((> (second (seventh hs1)) (second (seventh hs2))) 1)  ;; higher card
		 ((< (second (seventh hs1)) (second (seventh hs2))) -1) ;; higher card
;;		 ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
;;		 ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		 (t (BreakTies cvals1 cvals2))))   ;; hands equal
	  ;; one player has TwoPair
	  ((or (eighth hs1) (eighth hs2))
	   (cond ((null (eighth hs1)) -1)   ;; player2 has TofaK
		 ((null (eighth hs2))  1)   ;; player1 has TofaK
		 ((> (second (eighth hs1)) (second (eighth hs2))) 1)  ;; higher hard card
		 ((< (second (eighth hs1)) (second (eighth hs2))) -1) ;; higher high card
		 ((> (third (eighth hs1)) (third (eighth hs2))) 1)  ;; higher low card
		 ((< (third (eighth hs1)) (third (eighth hs2))) -1) ;; higher low card
;;		 ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
;;		 ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		 (t (BreakTies cvals1 cvals2))))  ;; hands equal
	  ;; one player has a Pair
	  ((or (ninth hs1) (ninth hs2))
	   (cond ((null (ninth hs1)) -1)   ;; player2 has Pair
		 ((null (ninth hs2))  1)   ;; player1 has Pair
		 ((> (second (ninth hs1)) (second (ninth hs2))) 1)  ;; higher card
		 ((< (second (ninth hs1)) (second (ninth hs2))) -1) ;; higher card
;;		 ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
;;		 ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		 (t (BreakTies cvals1 cvals2))))  ;; hands equal

	  ;; the strongest card wins
	  (t (cond ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
		   ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		   (t (BreakTies (reverse cvals1) (reverse cvals2))))))))  ;; hands equal
	   

(setf sorted1000 '(((2 D) (3 D)) ((3 C) (2 D)) ((2 C) (3 D)) ((2 C) (3 C)) ((3 H) (2 D)) ((3 H) (2 C)) ((2 H) (3 D)) ((2 H) (3 C))
 ((2 H) (3 H)) ((3 S) (2 D)) ((3 S) (2 C)) ((3 S) (2 H)) ((2 S) (3 D)) ((2 S) (3 C)) ((2 S) (3 H)) ((2 S) (3 S))
 ((2 D) (4 D)) ((4 C) (2 D)) ((2 C) (4 D)) ((2 C) (4 C)) ((4 H) (2 D)) ((4 H) (2 C)) ((2 H) (4 D)) ((2 H) (4 C))
 ((2 H) (4 H)) ((4 S) (2 D)) ((4 S) (2 C)) ((4 S) (2 H)) ((2 S) (4 D)) ((2 S) (4 C)) ((2 S) (4 H)) ((2 S) (4 S))
 ((2 D) (5 D)) ((5 C) (2 D)) ((2 C) (5 D)) ((2 C) (5 C)) ((5 H) (2 D)) ((5 H) (2 C)) ((2 H) (5 D)) ((2 H) (5 C))
 ((2 H) (5 H)) ((5 S) (2 D)) ((5 S) (2 C)) ((5 S) (2 H)) ((2 S) (5 D)) ((2 S) (5 C)) ((2 S) (5 H)) ((2 S) (5 S))
 ((2 D) (6 D)) ((6 C) (2 D)) ((2 C) (6 D)) ((2 C) (6 C)) ((6 H) (2 D)) ((6 H) (2 C)) ((2 H) (6 D)) ((2 H) (6 C))
 ((2 H) (6 H)) ((6 S) (2 D)) ((6 S) (2 C)) ((6 S) (2 H)) ((2 S) (6 D)) ((2 S) (6 C)) ((2 S) (6 H)) ((2 S) (6 S))
 ((2 D) (7 D)) ((7 C) (2 D)) ((2 C) (7 D)) ((2 C) (7 C)) ((7 H) (2 D)) ((7 H) (2 C)) ((2 H) (7 D)) ((2 H) (7 C))
 ((2 H) (7 H)) ((7 S) (2 D)) ((7 S) (2 C)) ((7 S) (2 H)) ((2 S) (7 D)) ((2 S) (7 C)) ((2 S) (7 H)) ((2 S) (7 S))
 ((2 D) (8 D)) ((8 C) (2 D)) ((2 C) (8 D)) ((2 C) (8 C)) ((8 H) (2 D)) ((8 H) (2 C)) ((2 H) (8 D)) ((2 H) (8 C))
 ((2 H) (8 H)) ((8 S) (2 D)) ((8 S) (2 C)) ((8 S) (2 H)) ((2 S) (8 D)) ((2 S) (8 C)) ((2 S) (8 H)) ((2 S) (8 S))
 ((2 D) (9 D)) ((9 C) (2 D)) ((2 C) (9 D)) ((2 C) (9 C)) ((9 H) (2 D)) ((9 H) (2 C)) ((2 H) (9 D)) ((2 H) (9 C))
 ((2 H) (9 H)) ((9 S) (2 D)) ((9 S) (2 C)) ((9 S) (2 H)) ((2 S) (9 D)) ((2 S) (9 C)) ((2 S) (9 H)) ((2 S) (9 S))
 ((2 D) (10 D)) ((10 C) (2 D)) ((2 C) (10 D)) ((2 C) (10 C)) ((10 H) (2 D)) ((10 H) (2 C)) ((2 H) (10 D)) ((2 H) (10 C))
 ((2 H) (10 H)) ((10 S) (2 D)) ((10 S) (2 C)) ((10 S) (2 H)) ((2 S) (10 D)) ((2 S) (10 C)) ((2 S) (10 H)) ((2 S) (10 S))
 ((J H) (2 D)) ((J S) (2 D)) ((2 H) (J D)) ((2 S) (J D)) ((2 S) (J C)) ((2 D) (J D)) ((J C) (2 D)) ((2 C) (J D))
 ((2 C) (J C)) ((J H) (2 C)) ((2 H) (J C)) ((2 H) (J H)) ((2 S) (J H)) ((2 S) (J S)) ((3 H) (4 H)) ((3 D) (4 D))
 ((3 C) (4 C)) ((4 C) (3 D)) ((3 C) (4 D)) ((4 H) (3 D)) ((4 H) (3 C)) ((3 H) (4 D)) ((3 H) (4 C)) ((4 S) (3 D))
 ((4 S) (3 H)) ((3 S) (4 D)) ((3 S) (4 C)) ((3 S) (4 H)) ((3 S) (4 S)) ((4 S) (3 C)) ((3 H) (5 H)) ((3 D) (5 D))
 ((5 C) (3 D)) ((3 C) (5 D)) ((5 S) (3 D)) ((5 S) (3 C)) ((5 S) (3 H)) ((3 S) (5 D)) ((3 S) (5 C)) ((5 H) (3 D))
 ((3 H) (6 H)) ((3 D) (6 D)) ((6 C) (3 D)) ((3 C) (6 D)) ((3 C) (6 C)) ((3 D) (7 D)) ((J H) (3 C)) ((3 D) (J D))
 ((4 D) (5 D)) ((4 D) (6 D)) ((4 C) (6 C)) ((6 C) (4 D)) ((4 H) (6 H)) ((6 H) (4 D)) ((4 D) (7 D)) ((4 C) (7 C))
 ((7 C) (4 D)) ((4 C) (7 D)) ((4 H) (7 H)) ((7 H) (4 D)) ((7 H) (4 C)) ((4 H) (7 D)) ((7 H) (8 D)) ((7 H) (9 D))
 ((7 H) (9 C)) ((Q S) (2 H)) ((5 H) (3 C)) ((3 H) (5 D)) ((3 H) (5 C)) ((3 S) (5 H)) ((3 S) (5 S)) ((6 S) (3 D))
 ((6 S) (3 C)) ((3 S) (6 D)) ((6 S) (3 H)) ((3 S) (6 S)) ((6 H) (3 D)) ((6 H) (3 C)) ((3 H) (6 D)) ((3 H) (6 C))
 ((7 S) (3 D)) ((3 S) (7 D)) ((3 S) (7 H)) ((7 C) (3 D)) ((3 C) (7 D)) ((3 C) (7 C)) ((7 S) (3 C)) ((7 S) (3 H))
 ((3 S) (7 C)) ((3 S) (7 S)) ((7 H) (3 D)) ((7 H) (3 C)) ((3 H) (7 D)) ((3 H) (7 C)) ((3 H) (7 H)) ((3 S) (8 D))
 ((3 S) (8 C)) ((3 S) (8 H)) ((3 S) (8 S)) ((3 C) (8 D)) ((3 C) (8 C)) ((3 D) (8 D)) ((8 C) (3 D)) ((8 H) (3 D))
 ((8 H) (3 C)) ((3 H) (8 D)) ((3 H) (8 C)) ((3 H) (8 H)) ((J S) (2 C)) ((J S) (2 H)) ((8 S) (3 D)) ((8 S) (3 C))
 ((8 S) (3 H)) ((3 S) (9 H)) ((3 S) (9 D)) ((3 S) (9 C)) ((3 S) (9 S)) ((3 C) (9 D)) ((3 C) (9 C)) ((3 D) (9 D))
 ((9 C) (3 D)) ((9 H) (3 D)) ((9 H) (3 C)) ((3 H) (9 D)) ((3 H) (9 C)) ((3 H) (9 H)) ((9 S) (3 D)) ((9 S) (3 C))
 ((9 S) (3 H)) ((3 S) (10 H)) ((3 S) (10 S)) ((3 S) (10 D)) ((3 S) (10 C)) ((3 D) (10 D)) ((3 C) (10 D)) ((3 C) (10 C))
 ((10 C) (3 D)) ((10 H) (3 D)) ((10 H) (3 C)) ((3 H) (10 D)) ((3 H) (10 C)) ((3 H) (10 H)) ((10 S) (3 D)) ((10 S) (3 C))
 ((10 S) (3 H)) ((3 S) (J H)) ((3 S) (J S)) ((3 S) (J D)) ((3 S) (J C)) ((J C) (3 D)) ((3 C) (J D)) ((3 C) (J C))
 ((J H) (3 D)) ((3 H) (J D)) ((J S) (3 D)) ((3 H) (J H)) ((J S) (3 C)) ((J S) (3 H)) ((5 C) (4 D)) ((4 C) (5 D))
 ((5 S) (4 D)) ((5 S) (4 C)) ((5 S) (4 H)) ((4 S) (5 D)) ((4 S) (5 C)) ((4 S) (5 H)) ((4 S) (5 S)) ((4 C) (5 C))
 ((5 H) (4 D)) ((5 H) (4 C)) ((4 H) (5 D)) ((4 H) (5 C)) ((4 C) (6 D)) ((6 S) (4 D)) ((6 S) (4 C)) ((6 S) (4 H))
 ((4 S) (6 D)) ((4 S) (6 C)) ((4 S) (6 H)) ((4 S) (6 S)) ((6 H) (4 C)) ((4 H) (6 D)) ((4 H) (6 C)) ((4 H) (7 C))
 ((7 S) (4 D)) ((7 S) (4 C)) ((7 S) (4 H)) ((4 S) (7 D)) ((4 S) (7 C)) ((4 S) (7 H)) ((4 S) (7 S)) ((4 D) (8 D))
 ((4 C) (8 C)) ((4 C) (8 D)) ((4 H) (8 H)) ((4 H) (8 D)) ((4 H) (8 C)) ((4 S) (8 D)) ((4 S) (8 C)) ((4 S) (8 H))
 ((4 S) (8 S)) ((8 C) (4 D)) ((8 H) (4 D)) ((8 H) (4 C)) ((8 S) (4 D)) ((8 S) (4 C)) ((8 S) (4 H)) ((4 C) (9 C))
 ((4 C) (9 D)) ((4 H) (9 H)) ((4 H) (9 D)) ((4 H) (9 C)) ((4 D) (9 D)) ((9 C) (4 D)) ((9 H) (4 D)) ((4 S) (9 D))
 ((4 S) (9 C)) ((4 S) (9 H)) ((9 H) (4 C)) ((9 S) (4 D)) ((9 S) (4 C)) ((9 S) (4 H)) ((4 D) (10 D)) ((4 C) (10 D))
 ((4 C) (10 C)) ((4 H) (10 H)) ((4 H) (10 D)) ((4 H) (10 C)) ((10 C) (4 D)) ((10 H) (4 D)) ((10 H) (4 C)) ((4 S) (10 D))
 ((4 S) (10 C)) ((4 S) (10 H)) ((10 S) (4 D)) ((3 H) (J C)) ((10 S) (4 C)) ((10 S) (4 H)) ((J H) (4 C)) ((4 D) (J D))
 ((J C) (4 D)) ((4 C) (J D)) ((4 C) (J C)) ((4 S) (J S)) ((4 H) (J D)) ((4 H) (J C)) ((J H) (4 D)) ((4 S) (J D))
 ((4 S) (J C)) ((4 S) (J H)) ((J S) (4 D)) ((J S) (4 H)) ((5 C) (6 C)) ((5 H) (6 C)) ((6 C) (5 D)) ((5 D) (6 D))
 ((5 C) (6 D)) ((6 H) (5 D)) ((6 H) (5 C)) ((6 S) (5 D)) ((6 S) (5 C)) ((5 H) (6 D)) ((6 S) (5 H)) ((5 S) (6 D))
 ((5 S) (6 C)) ((5 S) (6 H)) ((5 S) (6 S)) ((5 C) (7 C)) ((5 H) (7 C)) ((7 C) (5 D)) ((5 C) (7 D)) ((5 D) (7 D))
 ((7 S) (5 D)) ((7 S) (5 C)) ((7 H) (5 D)) ((7 H) (5 C)) ((5 H) (7 D)) ((5 S) (7 H)) ((5 S) (7 S)) ((5 C) (8 C))
 ((5 H) (8 H)) ((5 S) (8 S)) ((5 C) (8 D)) ((5 D) (8 D)) ((8 C) (5 D)) ((5 S) (8 D)) ((4 H) (J H)) ((5 H) (8 D))
 ((5 H) (8 C)) ((8 H) (5 D)) ((8 H) (5 C)) ((8 S) (5 D)) ((8 S) (5 C)) ((8 S) (5 H)) ((5 C) (9 C)) ((5 H) (9 H))
 ((5 S) (9 S)) ((5 D) (9 D)) ((9 C) (5 D)) ((5 C) (9 D)) ((5 H) (9 D)) ((5 H) (9 C)) ((5 S) (9 D)) ((5 S) (9 C))
 ((9 H) (5 D)) ((9 H) (5 C)) ((7 H) (9 H)) ((7 C) (10 C)) ((7 H) (10 D)) ((2 S) (Q H)) ((3 C) (5 C)) ((J S) (4 C))
 ((9 S) (5 D)) ((9 S) (5 C)) ((9 S) (5 H)) ((5 S) (10 S)) ((5 S) (10 D)) ((5 S) (10 C)) ((5 S) (10 H)) ((5 D) (10 D))
 ((10 C) (5 D)) ((5 C) (10 D)) ((5 C) (10 C)) ((10 H) (5 D)) ((10 H) (5 C)) ((10 S) (5 D)) ((10 S) (5 C)) ((10 S) (5 H))
 ((J H) (5 C)) ((5 H) (J D)) ((5 H) (J C)) ((7 H) (10 C)) ((7 H) (10 H)) ((7 S) (10 C)) ((7 S) (10 H)) ((7 S) (10 S))
 ((7 C) (J C)) ((7 H) (J D)) ((7 H) (J C)) ((J H) (7 D)) ((8 S) (9 H)) ((2 S) (Q D)) ((J H) (5 D)) ((J S) (5 D))
 ((J S) (5 C)) ((9 H) (8 D)) ((9 H) (8 C)) ((2 C) (Q D)) ((J S) (5 H)) ((6 D) (7 D)) ((2 S) (Q C)) ((6 H) (7 H))
 ((2 S) (Q S)) ((3 S) (6 C)) ((3 S) (6 H)) ((8 H) (9 D)) ((8 C) (10 C)) ((10 H) (8 D)) ((2 C) (Q C)) ((7 H) (6 D))
 ((6 S) (7 S)) ((6 H) (7 D)) ((5 S) (J S)) ((6 H) (7 C)) ((7 S) (6 D)) ((7 S) (6 C)) ((7 S) (6 H)) ((6 S) (7 D))
 ((10 H) (8 C)) ((8 H) (10 D)) ((8 S) (10 H)) ((3 H) (Q H)) ((6 C) (8 C)) ((6 D) (8 D)) ((6 H) (8 H)) ((6 H) (8 D))
 ((6 H) (8 C)) ((6 S) (8 S)) ((6 S) (8 D)) ((6 S) (8 C)) ((6 S) (8 H)) ((8 S) (6 C)) ((8 S) (6 H)) ((6 C) (9 C))
 ((3 C) (Q D)) ((5 H) (10 D)) ((5 H) (10 C)) ((5 S) (J D)) ((5 S) (J C)) ((5 S) (J H)) ((6 D) (9 D)) ((6 H) (9 D))
 ((6 H) (9 C)) ((8 S) (10 S)) ((10 S) (8 D)) ((8 D) (J D)) ((J H) (8 D)) ((J H) (8 C)) ((8 S) (J H)) ((10 H) (9 D))
 ((3 S) (Q C)) ((4 S) (9 S)) ((4 S) (10 S)) ((7 S) (5 H)) ((5 S) (7 D)) ((5 D) (J D)) ((J C) (5 D)) ((5 C) (J D))
 ((5 C) (J C)) ((6 H) (9 H)) ((10 H) (9 C)) ((9 H) (10 D)) ((9 D) (J D)) ((J H) (9 D)) ((J H) (9 C)) ((J S) (10 C))
 ((10 S) (J C)) ((3 H) (Q C)) ((5 S) (7 C)) ((5 S) (8 C)) ((6 S) (7 C)) ((6 S) (7 H)) ((7 C) (6 D)) ((6 C) (7 D))
 ((6 C) (7 C)) ((8 C) (6 D)) ((6 C) (8 D)) ((6 S) (9 S)) ((10 D) (J D)) ((3 C) (Q C)) ((Q S) (4 D)) ((5 S) (9 H))
 ((8 H) (6 D)) ((8 S) (6 D)) ((6 S) (9 D)) ((6 S) (9 C)) ((6 S) (9 H)) ((9 C) (6 D)) ((6 C) (9 D)) ((9 H) (6 D))
 ((9 S) (6 D)) ((9 H) (6 C)) ((9 S) (6 C)) ((6 C) (10 C)) ((6 D) (10 D)) ((6 H) (10 D)) ((6 H) (10 C)) ((6 H) (10 H))
 ((6 S) (10 S)) ((6 S) (10 D)) ((6 S) (10 C)) ((6 S) (10 H)) ((10 S) (6 D)) ((10 S) (6 C)) ((6 H) (J C)) ((6 H) (J H))
 ((6 D) (J D)) ((J H) (6 D)) ((3 H) (Q D)) ((6 H) (J D)) ((J H) (6 C)) ((6 S) (J D)) ((6 S) (J C)) ((6 S) (J H))
 ((3 S) (Q D)) ((10 C) (6 D)) ((6 C) (10 D)) ((Q H) (2 D)) ((10 H) (6 D)) ((Q S) (4 C)) ((10 H) (6 C)) ((10 S) (6 H))
 ((6 S) (J S)) ((J S) (6 D)) ((Q S) (4 H)) ((J S) (6 C)) ((Q H) (5 C)) ((J S) (6 H)) ((J C) (6 D)) ((6 C) (J D))
 ((6 C) (J C)) ((7 D) (8 D)) ((7 S) (8 S)) ((8 C) (7 D)) ((7 C) (8 D)) ((7 C) (8 C)) ((8 H) (7 D)) ((8 S) (7 D))
 ((8 S) (7 C)) ((8 S) (7 H)) ((8 H) (7 C)) ((7 S) (9 D)) ((7 S) (9 C)) ((7 S) (9 H)) ((7 D) (9 D)) ((9 C) (7 D))
 ((9 S) (7 D)) ((7 C) (9 D)) ((7 C) (9 C)) ((9 S) (7 C)) ((9 S) (7 H)) ((7 C) (10 D)) ((10 C) (7 D)) ((10 S) (7 D))
 ((10 S) (7 C)) ((10 S) (7 H)) ((9 H) (10 H)) ((3 S) (Q S)) ((4 H) (5 H)) ((4 S) (Q H)) ((5 S) (8 H)) ((3 S) (Q H))
 ((8 H) (6 C)) ((9 S) (6 H)) ((4 S) (Q S)) ((Q S) (5 D)) ((Q S) (5 C)) ((Q S) (5 H)) ((5 D) (Q D)) ((8 H) (10 C))
 ((8 C) (J C)) ((6 S) (Q H)) ((7 H) (8 H)) ((7 S) (8 D)) ((7 S) (8 C)) ((7 S) (8 H)) ((9 H) (7 D)) ((9 H) (7 C))
 ((7 S) (9 S)) ((7 D) (10 D)) ((10 H) (7 D)) ((10 H) (7 C)) ((7 S) (10 D)) ((7 D) (J D)) ((J S) (7 C)) ((J C) (7 D))
 ((J S) (7 D)) ((J S) (7 H)) ((8 S) (10 C)) ((8 H) (J D)) ((8 H) (J C)) ((J C) (8 D)) ((Q S) (2 C)) ((7 S) (J D))
 ((Q H) (3 D)) ((5 C) (Q D)) ((9 C) (10 C)) ((3 D) (Q D)) ((4 D) (Q D)) ((4 C) (Q D)) ((4 C) (Q C)) ((Q H) (4 D))
 ((4 H) (Q H)) ((5 H) (6 H)) ((5 H) (7 H)) ((5 H) (10 H)) ((5 H) (J H)) ((4 S) (Q D)) ((5 H) (Q D)) ((7 C) (J D))
 ((9 S) (8 D)) ((4 S) (Q C)) ((9 C) (8 D)) ((Q H) (6 D)) ((9 S) (8 H)) ((8 S) (J S)) ((J S) (8 D)) ((4 H) (Q C))
 ((8 S) (9 D)) ((5 H) (Q C)) ((8 S) (9 C)) ((8 S) (9 S)) ((10 C) (8 D)) ((8 C) (10 D)) ((5 H) (Q H)) ((8 C) (J D))
 ((2 D) (Q D)) ((4 H) (Q D)) ((5 S) (Q D)) ((5 S) (Q C)) ((5 S) (Q H)) ((8 C) (9 D)) ((9 S) (8 C)) ((5 S) (Q S))
 ((7 H) (6 C)) ((Q H) (6 C)) ((8 H) (J H)) ((9 H) (10 C)) ((10 S) (9 D)) ((10 S) (9 C)) ((10 S) (9 H)) ((9 S) (10 C))
 ((J S) (8 C)) ((J S) (8 H)) ((6 S) (Q C)) ((J H) (7 C)) ((8 D) (9 D)) ((8 D) (10 D)) ((8 S) (10 D)) ((9 S) (10 H))
 ((9 S) (10 D)) ((9 C) (10 D)) ((9 H) (J D)) ((5 C) (Q C)) ((9 H) (J C)) ((10 C) (J C)) ((J H) (10 D)) ((10 H) (J C))
 ((J S) (10 D)) ((Q H) (2 C)) ((10 S) (J D)) ((Q S) (2 D)) ((Q H) (5 D)) ((J H) (10 C)) ((6 S) (Q D)) ((6 S) (Q S))
 ((10 H) (J H)) ((Q S) (3 D)) ((Q S) (3 C)) ((Q S) (3 H)) ((Q C) (5 D)) ((8 H) (10 H)) ((Q S) (6 D)) ((Q S) (6 C))
 ((Q S) (6 H)) ((6 D) (Q D)) ((Q C) (6 D)) ((6 C) (Q C)) ((7 H) (J H)) ((Q H) (7 D)) ((8 H) (9 C)) ((10 S) (8 C))
 ((8 S) (J D)) ((9 S) (10 S)) ((J S) (9 D)) ((J S) (9 C)) ((10 H) (J D)) ((Q H) (3 C)) ((Q H) (4 C)) ((6 C) (Q D))
 ((8 S) (J C)) ((J S) (9 H)) ((9 S) (J D)) ((9 S) (J C)) ((Q H) (7 C)) ((Q S) (7 D)) ((9 S) (J H)) ((9 S) (J S))
 ((J C) (9 D)) ((9 C) (J D)) ((J C) (10 D)) ((10 C) (J D)) ((2 H) (Q D)) ((2 H) (Q C)) ((Q C) (2 D)) ((Q C) (3 D))
 ((10 S) (J H)) ((2 H) (Q H)) ((6 H) (Q D)) ((7 S) (J H)) ((7 S) (J S)) ((Q S) (7 C)) ((Q S) (7 H)) ((8 C) (9 C))
 ((10 S) (8 H)) ((7 S) (J C)) ((10 S) (J S)) ((Q C) (4 D)) ((6 H) (Q C)) ((7 H) (8 C)) ((8 H) (9 H)) ((6 H) (Q H))
 ((7 D) (Q D)) ((Q C) (7 D)) ((7 C) (Q D)) ((7 C) (Q C)) ((7 H) (Q D)) ((7 H) (Q C)) ((7 H) (Q H)) ((7 S) (Q D))
 ((7 S) (Q C)) ((7 S) (Q H)) ((9 D) (10 D)) ((10 C) (9 D)) ((9 C) (J C)) ((7 S) (Q S)) ((8 D) (Q D)) ((Q C) (8 D))
 ((Q H) (8 D)) ((Q H) (8 C)) ((9 H) (J H)) ((8 H) (Q D)) ((8 H) (Q C)) ((8 H) (Q H)) ((8 C) (Q D)) ((8 C) (Q C))
 ((Q S) (8 C)) ((Q S) (8 H)) ((Q S) (8 D)) ((8 S) (Q D)) ((8 S) (Q C)) ((8 S) (Q H)) ((8 S) (Q S)) ((9 D) (Q D))
 ((Q C) (9 D)) ((9 C) (Q D)) ((9 C) (Q C)) ((Q H) (9 D)) ((Q H) (9 C)) ((9 H) (Q D)) ((9 H) (Q C)) ((Q S) (9 D))
 ((Q S) (9 C)) ((Q S) (9 H)) ((J S) (10 H)) ((9 H) (Q H)) ((9 S) (Q D)) ((9 S) (Q C)) ((9 S) (Q H)) ((9 S) (Q S))
 ((10 D) (Q D)) ((Q C) (10 D)) ((10 C) (Q D)) ((10 C) (Q C)) ((Q H) (10 D)) ((Q H) (10 C)) ((10 H) (Q D)) ((10 H) (Q C))
 ((10 H) (Q H)) ((Q S) (10 D)) ((Q S) (10 C)) ((Q S) (10 H)) ((10 S) (Q D)) ((10 S) (Q C)) ((10 S) (Q H)) ((10 S) (Q S))
 ((J D) (Q D)) ((Q C) (J D)) ((J C) (Q D)) ((J C) (Q C)) ((Q H) (J D)) ((Q H) (J C)) ((J H) (Q D)) ((J H) (Q C))
 ((J H) (Q H)) ((Q S) (J D)) ((Q S) (J C)) ((Q S) (J H)) ((J S) (Q D)) ((J S) (Q C)) ((J S) (Q H)) ((J S) (Q S))
 ((2 D) (K D)) ((K C) (2 D)) ((2 C) (K D)) ((2 C) (K C)) ((K H) (2 D)) ((K H) (2 C)) ((2 H) (K D)) ((2 H) (K C))
 ((2 H) (K H)) ((K S) (2 D)) ((K S) (2 C)) ((K S) (2 H)) ((2 S) (K D)) ((2 S) (K C)) ((2 S) (K H)) ((2 S) (K S))
 ((3 D) (K D)) ((K C) (3 D)) ((3 C) (K D)) ((3 C) (K C)) ((K H) (3 D)) ((K H) (3 C)) ((3 H) (K D)) ((3 H) (K C))
 ((3 H) (K H)) ((K S) (3 D)) ((K S) (3 C)) ((K S) (3 H)) ((3 S) (K D)) ((3 S) (K C)) ((3 S) (K H)) ((3 S) (K S))
 ((4 D) (K D)) ((K C) (4 D)) ((4 C) (K D)) ((4 C) (K C)) ((K H) (4 D)) ((K H) (4 C)) ((4 H) (K D)) ((4 H) (K C))
 ((4 H) (K H)) ((K S) (4 D)) ((K S) (4 C)) ((K S) (4 H)) ((4 S) (K D)) ((4 S) (K C)) ((4 S) (K H)) ((4 S) (K S))
 ((5 D) (K D)) ((K C) (5 D)) ((5 C) (K D)) ((5 C) (K C)) ((K H) (5 D)) ((K H) (5 C)) ((5 H) (K D)) ((5 H) (K C))
 ((5 H) (K H)) ((K S) (5 D)) ((K S) (5 C)) ((K S) (5 H)) ((5 S) (K D)) ((5 S) (K C)) ((5 S) (K H)) ((5 S) (K S))
 ((6 D) (K D)) ((K C) (6 D)) ((6 C) (K D)) ((6 C) (K C)) ((K H) (6 D)) ((K H) (6 C)) ((6 H) (K D)) ((6 H) (K C))
 ((6 H) (K H)) ((K S) (6 D)) ((K S) (6 C)) ((K S) (6 H)) ((6 S) (K D)) ((6 S) (K C)) ((6 S) (K H)) ((6 S) (K S))
 ((7 D) (K D)) ((K C) (7 D)) ((7 C) (K D)) ((7 C) (K C)) ((K H) (7 D)) ((K H) (7 C)) ((7 H) (K D)) ((7 H) (K C))
 ((7 H) (K H)) ((K S) (7 D)) ((K S) (7 C)) ((K S) (7 H)) ((7 S) (K D)) ((7 S) (K C)) ((7 S) (K H)) ((7 S) (K S))
 ((8 D) (K D)) ((K C) (8 D)) ((8 C) (K D)) ((8 C) (K C)) ((K H) (8 D)) ((K H) (8 C)) ((8 H) (K D)) ((8 H) (K C))
 ((8 H) (K H)) ((K S) (8 D)) ((K S) (8 C)) ((K S) (8 H)) ((8 S) (K D)) ((8 S) (K C)) ((8 S) (K H)) ((8 S) (K S))
 ((9 D) (K D)) ((K C) (9 D)) ((9 C) (K D)) ((9 C) (K C)) ((K H) (9 D)) ((K H) (9 C)) ((9 H) (K D)) ((9 H) (K C))
 ((9 H) (K H)) ((K S) (9 D)) ((K S) (9 C)) ((K S) (9 H)) ((9 S) (K D)) ((9 S) (K C)) ((9 S) (K H)) ((9 S) (K S))
 ((10 D) (K D)) ((K C) (10 D)) ((10 C) (K D)) ((10 C) (K C)) ((K H) (10 D)) ((K H) (10 C)) ((10 H) (K D)) ((10 H) (K C))
 ((10 H) (K H)) ((K S) (10 D)) ((K S) (10 C)) ((K S) (10 H)) ((10 S) (K D)) ((10 S) (K C)) ((10 S) (K H)) ((10 S) (K S))
 ((J D) (K D)) ((K C) (J D)) ((J C) (K D)) ((J C) (K C)) ((K H) (J D)) ((K H) (J C)) ((J H) (K D)) ((J H) (K C))
 ((J H) (K H)) ((K S) (J D)) ((K S) (J C)) ((K S) (J H)) ((J S) (K D)) ((J S) (K C)) ((J S) (K H)) ((J S) (K S))
 ((Q D) (K D)) ((K C) (Q D)) ((Q C) (K D)) ((Q C) (K C)) ((K H) (Q D)) ((K H) (Q C)) ((Q H) (K D)) ((Q H) (K C))
 ((Q H) (K H)) ((K S) (Q D)) ((K S) (Q C)) ((K S) (Q H)) ((Q S) (K D)) ((Q S) (K C)) ((Q S) (K H)) ((Q S) (K S))
 ((A D) (2 D)) ((2 C) (A D)) ((A C) (2 D)) ((A C) (2 C)) ((2 H) (A D)) ((2 H) (A C)) ((A H) (2 D)) ((A H) (2 C))
 ((A H) (2 H)) ((2 S) (A D)) ((2 S) (A C)) ((2 S) (A H)) ((A S) (2 D)) ((A S) (2 C)) ((A S) (2 H)) ((A S) (2 S))
 ((A D) (3 D)) ((3 C) (A D)) ((A C) (3 D)) ((A C) (3 C)) ((3 H) (A D)) ((3 H) (A C)) ((A H) (3 D)) ((A H) (3 C))
 ((A H) (3 H)) ((3 S) (A D)) ((3 S) (A C)) ((3 S) (A H)) ((A S) (3 D)) ((A S) (3 C)) ((A S) (3 H)) ((A S) (3 S))
 ((A D) (4 D)) ((4 C) (A D)) ((A C) (4 D)) ((A C) (4 C)) ((4 H) (A D)) ((4 H) (A C)) ((A H) (4 D)) ((A H) (4 C))
 ((A H) (4 H)) ((4 S) (A D)) ((4 S) (A C)) ((4 S) (A H)) ((A S) (4 D)) ((A S) (4 C)) ((A S) (4 H)) ((A S) (4 S))
 ((A D) (5 D)) ((5 C) (A D)) ((A C) (5 D)) ((A C) (5 C)) ((5 H) (A D)) ((5 H) (A C)) ((A H) (5 D)) ((A H) (5 C))
 ((A H) (5 H)) ((5 S) (A D)) ((5 S) (A C)) ((5 S) (A H)) ((A S) (5 D)) ((A S) (5 C)) ((A S) (5 H)) ((A S) (5 S))
 ((A D) (6 D)) ((6 C) (A D)) ((A C) (6 D)) ((A C) (6 C)) ((6 H) (A D)) ((6 H) (A C)) ((A H) (6 D)) ((A H) (6 C))
 ((A H) (6 H)) ((6 S) (A D)) ((6 S) (A C)) ((6 S) (A H)) ((A S) (6 D)) ((A S) (6 C)) ((A S) (6 H)) ((A S) (6 S))
 ((A D) (7 D)) ((7 C) (A D)) ((A C) (7 D)) ((A C) (7 C)) ((7 H) (A D)) ((7 H) (A C)) ((A H) (7 D)) ((A H) (7 C))
 ((A H) (7 H)) ((7 S) (A D)) ((7 S) (A C)) ((7 S) (A H)) ((A S) (7 D)) ((A S) (7 C)) ((A S) (7 H)) ((A S) (7 S))
 ((A D) (8 D)) ((8 C) (A D)) ((A C) (8 D)) ((A C) (8 C)) ((8 H) (A D)) ((8 H) (A C)) ((A H) (8 D)) ((A H) (8 C))
 ((A H) (8 H)) ((8 S) (A D)) ((8 S) (A C)) ((8 S) (A H)) ((A S) (8 D)) ((A S) (8 C)) ((A S) (8 H)) ((A S) (8 S))
 ((A D) (9 D)) ((9 C) (A D)) ((A C) (9 D)) ((A C) (9 C)) ((9 H) (A D)) ((9 H) (A C)) ((A H) (9 D)) ((A H) (9 C))
 ((A H) (9 H)) ((9 S) (A D)) ((9 S) (A C)) ((9 S) (A H)) ((A S) (9 D)) ((A S) (9 C)) ((A S) (9 H)) ((A S) (9 S))
 ((A D) (10 D)) ((10 C) (A D)) ((A C) (10 D)) ((A C) (10 C)) ((10 H) (A D)) ((10 H) (A C)) ((A H) (10 D)) ((A H) (10 C))
 ((A H) (10 H)) ((10 S) (A D)) ((10 S) (A C)) ((10 S) (A H)) ((A S) (10 D)) ((A S) (10 C)) ((A S) (10 H)) ((A S) (10 S))
 ((A D) (J D)) ((J C) (A D)) ((A C) (J D)) ((A C) (J C)) ((J H) (A D)) ((J H) (A C)) ((A H) (J D)) ((A H) (J C))
 ((A H) (J H)) ((J S) (A D)) ((J S) (A C)) ((J S) (A H)) ((A S) (J D)) ((A S) (J C)) ((A S) (J H)) ((A S) (J S))
 ((A D) (Q D)) ((Q C) (A D)) ((A C) (Q D)) ((A C) (Q C)) ((Q H) (A D)) ((Q H) (A C)) ((A H) (Q D)) ((A H) (Q C))
 ((A H) (Q H)) ((Q S) (A D)) ((Q S) (A C)) ((Q S) (A H)) ((A S) (Q D)) ((A S) (Q C)) ((A S) (Q H)) ((A S) (Q S))
 ((A D) (K D)) ((K C) (A D)) ((A C) (K D)) ((A C) (K C)) ((K H) (A D)) ((K H) (A C)) ((A H) (K D)) ((A H) (K C))
 ((A H) (K H)) ((K S) (A D)) ((K S) (A C)) ((K S) (A H)) ((A S) (K D)) ((A S) (K C)) ((A S) (K H)) ((A S) (K S))
 ((2 C) (2 D)) ((2 H) (2 D)) ((2 H) (2 C)) ((2 S) (2 D)) ((2 S) (2 C)) ((2 S) (2 H)) ((3 C) (3 D)) ((3 H) (3 D))
 ((3 H) (3 C)) ((3 S) (3 D)) ((3 S) (3 C)) ((3 S) (3 H)) ((4 C) (4 D)) ((4 H) (4 D)) ((4 H) (4 C)) ((4 S) (4 D))
 ((4 S) (4 C)) ((4 S) (4 H)) ((5 C) (5 D)) ((5 H) (5 D)) ((5 H) (5 C)) ((5 S) (5 D)) ((5 S) (5 C)) ((5 S) (5 H))
 ((6 C) (6 D)) ((6 H) (6 D)) ((6 H) (6 C)) ((6 S) (6 D)) ((6 S) (6 C)) ((6 S) (6 H)) ((7 C) (7 D)) ((7 H) (7 D))
 ((7 H) (7 C)) ((7 S) (7 D)) ((7 S) (7 C)) ((7 S) (7 H)) ((8 C) (8 D)) ((8 H) (8 D)) ((8 H) (8 C)) ((8 S) (8 D))
 ((8 S) (8 C)) ((8 S) (8 H)) ((9 C) (9 D)) ((9 H) (9 D)) ((9 H) (9 C)) ((9 S) (9 D)) ((9 S) (9 C)) ((9 S) (9 H))
 ((10 C) (10 D)) ((10 H) (10 D)) ((10 H) (10 C)) ((10 S) (10 D)) ((10 S) (10 C)) ((10 S) (10 H)) ((J C) (J D)) ((J H) (J D))
 ((J H) (J C)) ((J S) (J D)) ((J S) (J C)) ((J S) (J H)) ((Q C) (Q D)) ((Q H) (Q D)) ((Q H) (Q C)) ((Q S) (Q D))
 ((Q S) (Q C)) ((Q S) (Q H)) ((K C) (K D)) ((K H) (K D)) ((K H) (K C)) ((K S) (K D)) ((K S) (K C)) ((K S) (K H))
 ((A C) (A D)) ((A H) (A D)) ((A H) (A C)) ((A S) (A D)) ((A S) (A C)) ((A S) (A H))))

