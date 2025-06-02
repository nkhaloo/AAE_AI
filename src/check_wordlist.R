library(stringr)

# 1. Original word list
wordlist <- c("peat", "seat", "feet", "deep", "beat", "seek", "teeth", "peak", "sheet", "keep", "beep", "cheek",
              "kit", "ship", "tip", "fit", "pit", "pick", "tick", "sit", "chip", "sip", "bit", "dip",
              "tape", "cake", "cape", "date", "gate", "take", "safe", "fake", "fate", "shape", "bait", "sake",
              "dead", "get", "beg", "bet", "debt", "peck", "deck", "pet", "pep", "vet", "jet", "bed",
              "gap", "pack", "tap", "sat", "bat", "pat", "cap", "tat", "cat", "fat", "zap", "tax",
              "dot", "got", "pox", "tot", "jock", "shot", "sock", "dock", "cop", "pot", "not", "box",
              "soup", "duke", "dupe", "food", "boot", "loop", "kook", "tube", "tooth", "suit", "shoot", "goop", 
              "caught", "bought", "taught", "thought", "fought", "sought", "moss", "boss", "talk", "loss", "toss",
              "walk")

# 2. Full passage text
passage <- tolower("
I took a seat and heard a beep as I checked my teeth after I bit down. A sudden zap startled the lazy cat, light on its feet, making it leap onto the deck. It made me want to seek out some mice. I quickly grabbed my gardening kit and started digging through the peat in a shallow pit, careful not to step in the soft moss, or get anything on my cheek. My friend wanted to toss me some water to him. 
At the café, I ordered soup, a piece of cake, and a refreshing sip of water, feeling perfectly safe. Outside, a playful jock wearing one sock tossed a baseball bat, skipping a beat. Someone shouted, and I nearly shot out of my place.
In town, a cop helped me pick a gift—a sheet of goop—though I wondered if it was a dupe. I bought a small wooden box, and put it carefully in my pack. I felt like I was at my peak.
At home, I couldn't keep my eyes open and decided to sit on my soft bed, pulling on a warm boot. A quick tick from the clock reminded me of a forgotten date and an unpaid tax that had led to debt that was fit.
The strange fake duke wore a flowing cape, talking nonsense about his unlucky fate on a ship and how he had once fought over a mysterious dot on the tip of an old map. He has an old tat of a fat tooth holding a tube of toothpaste, and wearing a suit. He got it when he was caught in a storm before reaching a dock. Once, he said that had taken a dip in shark infested waters. Clearly, a kook, for goodness sake. 
I remembered I sought to talk with my boss about feeling dead tired. Instead, I had a bowl of food, watched a video about a chip, drifted to sleep, and dreamt about a big gate. I was awakened by the shape of someone trying to take my tape. I had to beg my coworker to go find more and replace the loss I was experiencing. 
I dreamed vividly. I saw a tiny tot crying over a lost toy. I also saw a vet making a bet drinking out of a water tap. I saw a jet landing, a duck taking a peck. I saw a skateboarder shooting the gap, a cheerleader with lots of pep, and a flock of birds fighting over bait. I saw a pet that sat, waiting for a pat. I saw a water bottle with no cap, and a baby with chicken pox running around the loop. I saw a garbage shoot and a pot of gold. Then the alarm sounded, loud and deep, and with one final sleepy thought, I woke. I went and took a walk, and found some water. The dream taught me some lessons. 

")

# 3. Tokenize and count
tokens <- str_extract_all(passage, "\\b[a-z]+\\b")[[1]]
counts <- table(factor(tokens, levels = wordlist))

# 4. Check for missing or duplicated words
missing <- names(counts[counts == 0])
duplicates <- names(counts[counts > 1])

# 5. Output results
if (length(missing) == 0 && length(duplicates) == 0) {
  cat("✅ All 84 words are used exactly once.\n")
} else {
  if (length(missing) > 0) {
    cat("❌ Missing words:\n")
    print(missing)
  }
  if (length(duplicates) > 0) {
    cat("⚠️ Duplicated words:\n")
    print(duplicates)
  }
}

1 + 1 
