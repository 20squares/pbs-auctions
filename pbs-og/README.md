# Table of contents
- [Summary](#summary)
    - [Analytics results](#analytics-results)
- [Installation](#installation)
    - [Normal execution](#normal-execution)
    - [Interactive execution](#interactive-execution)
    - [Addendum: Installing haskell](#addendum-installing-haskell)
        - [Installing through `nix` (recommended)](#installing-through-nix-recommended)
          - [Installing `nix`](#installing-nix)
          - [Setting up the environment](#setting-up-the-environment)
          - [Freeing space](#freeing-space)
        - [Installing through GHCup](#installing-through-ghcup)
- [Explaining the model](#explaining-the-model)
    - [Equilibrium vs. Simulations](#equilibrium-vs-simulations)
    - [Approximate equilibrium](#approximate-equilibrium)
    - [Markov games](#markov-games)
- [Code deep dive](#code-deep-dive)
    - [Recap: DSL primer](#recap-dsl-primer)
        - [The building blocks](#the-building-blocks)
        - [exogenous parameters](#exogenous-parameters)
        - [Basic operations](#basic-operations)
        - [Branching](#branching)
        - [Supplying strategies](#supplying-strategies)
            - [Evaluating strategies](#evaluating-strategies)
            - [Stochasticity](#stochasticity)
            - [Branching](#branching-1)
        - [Stochasticity](#stochasticity)
    - [File structure](#file-structure)
- [Analytics](#analytics)
    - [Reading the analytics](#reading-the-analytics)
    - [Strategies emplyed in the analysis](#strategies-employed-in-the-analysis)
    - [Running the analytics](#running-the-analytics)
    - [Results](#results)
      - [Static auctions](#static-auctions)
    - [Sanity checks](#sanity-checks)



# Summary

In this FRP we focused on modelling some of the thought experiments around Proposer-Builder Separation (PBS). Essentially, we implemented a modular bidding game and experimented with different auction procedures.


## Analytics results

The analytics results of this FRP are not particularly surprising, as dominant strategies for n-th price auctions and many dynamic auctions, when known, are already well-studied in the literature. As a consequence, we preferred focusing on simulations more than on equilibria: In a nutshell, this means that albeit this model can be used to verify that a given strategy for a given auction is an equilibrium, we believe that its best application would be to answer questions like:

"Given some auction design, and assuming that every player plays according to some fixed strategy, what is my expected outcome given a bidding strategy?"

Clearly, this FRP is just a proof of concept. However, we do believe that it can be evolved into a fully-flagged auction-simulation software suite, which may benefit different actors in PBS (and in the OFA ecosystem more in general) to better position themselves within the landscape.

More details can be found in [Results](#results).

The full battery of available analytics can be run by following the [Normal execution](#normal-execution) subsection. As equilibrium checking is quite computationally extensive, the full battery of analytics has been furthermore split in two: in [Interactive execution](#interactive-execution) mode, give:

- `onlyEquilibria` to just run the equilibrium checking part (computationally heavy);
- `onlySimulations` to just run the simulation part (computationally lightweight);
- `main` to run both.

Further information about how to run the analytics can be found at [Running the analytics](#running-the-analytics).


# Installation
To run the model, it is necessary to have `haskell` and `stack` installed on your machine. Refer to the subsection [Addendum: Installing haskell](#addendum-installing-haskell) for instructions. A deeper dive into the code structure can be found in the [Code deep dive](#code-deep-dive) subsection.

There are two main ways of running the model: normal and interactive execution.


## Normal execution
To 'just' run the model, type

```sh
stack run
```
in the main directory, where the file `stack.yaml` is located.
The model will be compiled and a predefined set of analytics will be run. The results of the predefined analytics will be shown on terminal.


## Interactive execution
One of the most powerful features of `haskell` is *REPL mode*. This allows you to recompile the code on the fly, to query the type of a function and a lot of other things. To start interactive mode, just run

```sh
stack ghci
```

in the main directory. The code will compile, and then an interactive terminal (REPL) window will open. There are various commands that can be fed to the REPL. Among the most useful ones we highlight:

| Command         | Description               |
|:----------------|--------------------------:|
| `:q`            | quit interactive mode     |
| `:r`            | recompile the source code |
| `:l module`     | load module               |
| `:t expression` | query expression type     |

Of these commands, `:t` is the most important one, as it allows to visualize clearly what type of input we must feed to a given function. For instance, `:t (&&)` produces the output:

```haskell
(&&) :: Bool -> Bool -> Bool
```
Which tells us that `(&&)` - the logical `and` operator - takes a boolean (a truth value), then another boolean, and returns a boolean (the logical `and` of the first two).

Since under the hood games are nothing more than functions, REPL allows us to see the game type by doing `:t gameName`. If the game is parametrized, say, over a string, then `:t gameName "string"` will return the type where the first component has already been filled.

This tool is expecially powerful to better understand the structure of the strategies we have to feed to the model, which can grow very complicated as the model scales.


## Addendum: Installing haskell

If you dont' have either `haskell` or `stack`, it is necessary to install them. There are many ways to do so, of which we propose two: The first one, which we recommend, is through the [`nix`](https://nixos.org/download.html) package manager. The second one, is via [`GHCup`](https://www.haskell.org/ghcup/).

### Installing through `nix` (recommended)

[`nix`](https://nixos.org/download.html) is a package manager that allows to build environments deterministically. This means that it offers the right granularity to set up a developing environment exactly as one wants it. All of our projects get shipped together with something called a *`nix` flake*, which is a set of instructions telling `nix` to install all needed dependencies precisely at the version we used during development. This drastically reduces the possibility of compiling/execution errors and it is why we strongly recommend using `nix`.

#### Installing `nix`

To install `nix`, follow the [official instructions](https://nixos.org/download.html) for your operating system. Please note that on windows this will require installing [WSL2](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux) first. 

`nix` flakes require enabling experimental features to work properly. To do so in a Unix-based system, type the following commands in a terminal:

```sh
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

On other operating systems the procedure may be different.

#### Setting up the environment

Now that `nix` is up and running, we can fire up the environment. In a terminal, navigate in the main repo folder, where `flake.nix` is. Before running any command, type

```sh
nix develop
```

This will create a reproducible ephemeral devshell exposing all the required dependencies for running the project (slack, ghc, haskell-language-sever). Please note that this will take around 7GB of space.

While in the devshell, you can proceed as in [Normal execution](#normal-execution) and [Interactive execution](#interactive-execution). When you're done trying out the model, you can type

```sh
exit
```

or close the terminal window to exit from the devshell.


#### Freeing space

If you do not plan to use the model in the foreseeable future and want to reclaim some hard-disk space, in a terminal (outside the `nix develop` environmnet) just give:

```sh
nix-collect-garbage
nix store optimise
```


### Installing through `GHCup`

Another way to set up the environment to run the project is via [`GHCup`](https://www.haskell.org/ghcup/).
In a terminal, type:

```sh
 curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh 
```

If asked, respond 'yes' (`Y`) to the following questions:

```
Do you want to install haskell-language-server (HLS)?
Do you want to enable better integration of stack with `GHCup`?
```

Afterwards, `GHCup` may ask you to install some additional packages before continuing with the installation. Follow the advice before continuing. Then, just follow the instructions through the end.

Again, the installation is quite massive in terms of space. In this respect, `GHCup` is a convenient solution in that it installs only in one folder (on Linux systems, `/home/$USER/.ghcup`). Should you decide to get rid of `haskell` altogether, just delete the folder.

Again, once `GHCup` has installed, you can proceed as in [Normal execution](#normal-execution) and [Interactive execution](#interactive-execution).

**A note of warning:** GHC, the `haskell` compiler installed with `GHCup`, relies heavily on the GCC compiler. GHC assumes that GCC comes together with all the relevant libraries. As such, in compiling the model you may get errors such as:

```sh
/usr/bin/ld.gold: error: cannot find -ltinfo
```

these errors hint at missing GCC libraries, which will have to be installed independently. The precise iter to do so depends on the libraries involved and on your operating system. Unfortunately there is little we can do about it, as this is a problem with the general `haskell` developer infrastructure.

The main way to avoid this is by using the recommended installation via [`nix`](#installing-through-nix-recommended).



# Explaining the model

Here, we give a more detailed explanation of what our model does.

Our model is simple and comprises two different actors:

- **Bidders**, which represent builders competing for blockspace. Each **Bidder** has a private valuation that is drawn from nature. This private valuation represents how much **Bidder** thinks that the blockspace on auction 'is really worth'. **Bidder** observes the private value and uses it to formulate a bid.
- **Proposer**, which observes the bids and awards the blockspace to a winning **Bidder**. Proposer is free to chose the auctioning procedure.

Moreover, we will sometimes make use of a third, non-strategic component, called **Relayer**: These are intermediate components that relay the bids to **Proposer**. A relayer observes bids from a subset of builders, and has to filter one **Bidder** from the subset, whose bid will be relayed to the **Proposer**. Each relayer is free to chose whatever procedure for the filtering. **Relayers** are not strategic actors, meaning that the selection they perform is completely mechanicistic.
In practice, when **Relayers** are used we implement a *nested auction*, since **Bidders** have to first compete to be selected by their **Relayer**, and then have to compete to be selected by **Proposer**. 

**Bidders** want to maximize their payoff, which is calculated as:
- A profit corresponding to the difference between the private valuation the **Bidder** has and how much the **Bidder** paid, in the case the **Bidder** wins the auction;
- A loss corresponding to the amount paid in the case the **Bidder** loses the auction. Notice that there are auctions in which all **Bidders** pay, not just the winning one!

Similarly, **Proposer** wants to maximize their payoff, which is just the amount that **Bidders** pay.

We instantiate the model in a very simple way. In the case named `Current Status quo` there are:
- Four **Bidders**;
- Two **Relayers**, each one managing a set of two **Bidders**. There is no overlap between these sets, that is, there is no **Bidder** submitting their bid to more than one **Relayer**.
- One **Proposer**, which has to choose between the two bids selected by **Relayers**.

In this model **Relayers** just pick the biggest bid out of the subset they manage, in a completely deterministic, non-strategic way. On the other hand, **Proposer** does not follow any auction procedure and can pick the winning bid following any possible strategy.

In the case named `Assembled auctions` instead there are:
- Four **Bidders**;
- One **Proposer**, which has no strategic content. **Proposer** here just runs an auction mechanically. We modelled different auction procedures for **Proposer**, namely:
  - First price auction, where the winning bid is the highest one. These auctions can have a reserve price, meaning that no **Bidder** wins the auction if the reserve price is not met. For now, we set this price to zero.
  - Second price auction, where the winning bid is the second highest one. Again, these auctions can have a reserve price, which we set to zero for now.
  - All-pay auctions, where all **Bidders** pay, not just the winning one.
  - We also modelled *dynamic auctions*. These are auctions where the information available to players is constantly updated, and players can dynamically adapt their bidding strategy over time. In particular, we implemented `Japanese auctions`, which work as follows:
    - There is a ticker displaying the current price. This is the bid value.
    - The ticker goes up by a fixed amount at every interval.
    - Each time the ticker goes up, **Bidders** must decide if they want to leave the auction or not.
    - If at time $t$ all players have left the auction, then the players in the auction at time $t-1$ are the possibly winning bidders.
    - The winning **Bidder** is chosen randomly from the possible winning bidders. In practice, this is implemented as a uniform probability distribution.
    - The winning **Bidder** has to pay the bid displayed at time $t$.


## Equilibrium Vs. Simulations

In this model, aside of equilibrium checking, we also provided simulation capabilities. This is defined in `Analytics.hs`, see [File structure](#file-structure) for more information. In a nutshell:

- Equilibrium checking takes a strategy, computes payoffs, and searches for any strategic deviation that would result in a better payoff for one of the players.
- Simulations just plays the strategy, without looking for profitable deviations.

The reason why we deem simulations important in this framework is because they can be used to quickly check how profitable a given strategy is. Simulations are also computationally lightweight compared to equilibrium checking.


## Approximate equilibrium

In some of the models we developed, such as the ones employing first price auctions, we adopted an approximate version of equilibrium, which discards profitable strategic deviations if profits are closer than a fixed $\epsilon$ to the current one. The reason for this is that some results around equilibria in auction theory depend on a hypothesis of continuity: Namely, it is postulated that some variables range within the real numbers. Computers force us to work in a discretized settings, the consequence being that this hypothesis cannot be satisfied in our model. Approximate equilibria allow us to 'blur' the line between discrete and continuous, and to obtain results in line with the ones 'pen and paper' auction theory would predict.


## Markov games

As explained in detail in [Stochasticity](#stochasticity), all of our games are by default Bayesian. This allows us to use mixed strategies and probabilistic reasoning out of the box. To implement dynamic auctions, we had to resort to something slightly more complicated, **Bayesian Markov games**. A Markov game is a repeated game where the information at round $n$ influences round $n+1$. To be more precise, at each round the game can evolve into multiple states. The evolution is modelled as a probabilistic transition function dependent on the state of the game at the previous round and on the strategies players played there. The reason why these games are called 'Markov' is because at round $n+1$ only what happens at round $n$ matters (players' strategies and game state), whereas all previous information is discarded.

In a Markov game that is furthermore Bayesian, players can reason counterfactually at the strategic level, and we can consider mixed strategies at each round. So, whereas Bayesian games are probabilistic at the strategic level, Markov games are probabilistic at the level of game state evolution. For these reasons the two can be combined.

We used Bayesian Markov games to model Japanese auctions (details at [Explaining the model](#explaining-the-model)): Here, at each round the game can probabilistically transition to two new games: One is the same game of the previous round with price incremented, whereas the other one is the empty game in which no strategies are played and payoffs are calculated.



# Code deep dive


## Recap: DSL primer

Our models are written in a custom DSL compiled to `haskell`. Here we give a brief description of how our software works.

### The building blocks

The basic building block of our model is called **open game**, and can be thought of as a game-theoretic lego brick. This may represent a player, a nature draw, a payoff matrix or a complex combination of these elements. It has the following form:

```haskell
gameName variables = [opengame|

   inputs    : a;
   feedback  : b;

   :----------------------------:

   inputs    : a';
   feedback  : b';
   operation : content;
   outputs   : s';
   returns   : t';

   :----------------------------:

   outputs   :  s;
   returns   :  t;
  |]
```

We can imagine this block as a box with 4 wires on its outside, on which travels information marked as:
- `inputs`, data that gets fed into the game (e.g. a player receiving information from a context).
- `outputs`, data that the game feeds to the outside world (e.g. a player communicating a choice to another player).
- `returns`, the returns of a player actions, which are usually directly fed to a function calculating payoffs.
- The `feedback` wire which sends information back in time. If, intuitively, `returns` represents the returns on a player action, one could imagine it as 'information that an agents receive from the future'. `feedback` is the dual analog of that: If a given piece of information comes from the future, someone in the future must have been sent it to the past. For additional details about the `feedback` wire please refer to the relevant [literature](https://arxiv.org/abs/1603.04641).

The `:--:` delimiters separate the outside from the inside of the box. As one can see, the interfaces inside are replicated. This is intentional as it allows for a notion of *nesting*. For instance, the situation depicted in the following picture:

![An open grame in graphical form](pics/box.png)

Can be represented by the following code block:

```haskell
gameName variables = [opengame|

   inputs    : a, a';
   feedback  : b;

   :----------------------------:

   inputs    : a';
   feedback  : ;
   operation : SubGame1;
   outputs   : x;
   returns   : t';

   inputs    : a, x;
   feedback  : b;
   operation : SubGame2;
   outputs   : s;
   returns   : t;
   :----------------------------:

   outputs   :  s;
   returns   :  t,t';
  |]
```

In turn, `Subgame1` and `Subgame2` can be other games defined using the same DSL. Notice that the wire `x` is internal and totally hidden from the 'outside world'. 

### exogenous parameters

An exogenous parameter is a given assumption that is not part of the model, and is fed to it externally. As such, it is treated by the model as a 'fact' that cannot really be modified. An example of exogenous parameter could be the market conditions at the time when a game is played.

exogenous parameters are just defined as variables, as the field `variables` in the previous code blocks testifes. These variables can in turn be fed as exogenous parameters to inside games, as in the following example:

```haskell
gameName stock1Price stock2Price  = [opengame|

   inputs    : a, a';
   feedback  : b;

   :----------------------------:

   inputs    : a';
   feedback  : ;
   operation : SubGame1 stock1Price;
   outputs   : x;
   returns   : t';

   inputs    : a, x;
   feedback  : b;
   operation : SubGame2 stock2Price;
   outputs   : s;
   returns   : t;
   :----------------------------:

   outputs   :  s;
   returns   :  t,t';
  |]
```

### Basic operations

In addition to the DSL defining the 'piping rules' between boxes, we provide some *basic operations* to populate a box, namely:
- A *function*, which just transforms the input in some output.
- A *stochastic distribution*, used to implement draws from nature.
- A *strategic choice*, which can be thought of as a function parametrized over strategies.

### Branching

Another important operation we provide is called *branching*. This is useful in contexts where, say, a player choice determines which subgame is going to be played next.
Branching is represented using the operator `+++`. So, for instance, if `SubGame1` is defined as ```branch1 +++ branch2```, then we are modelling a situation where `SubGame1` can actually evolve into two different games depending on input. As the input of a game can be the outcome of a strategic choice in some other game, this allows for flexible modelling of complex situations.

Graphically, branching can be represented by resorting to [sheet diagrams](https://arxiv.org/abs/2010.13361), but as they are quite complicated to draw, this depiction is rarely used in practice.

### Supplying strategies

As usual in classical game theory, a strategy conditions on the observables and assigns a (possibly randomized) action. 

Every player who can make a decision in the game needs to be assigned a strategy. These individual strategies then get aggregated into a list representing the complete strategy for the whole game.

So, for instance, if our model consists of three subgames, a strategy for the whole model will just be a list:

```haskell
strGame1 ::- strGame2 ::- strGame3 ::- Nil
```

#### Evaluating strategies

To evaluate strategies, it is enough to just run the `main` function defined in `Main.hs`. This is precisely what happens when we give the command `stack run`. In turn, `main` invokes functions defined in `Analysis.hs` which define the right notion of equilibrium to check. If you want to change strategies on the fly, just open a REPL (Cf. [Interactive Execution](#interactive-execution)) and give the command `main`.
You can make parametric changes or even define new strategies and/or notions of equilibrium by editing the relevant files (cf. [File structure](#file-structure)). Once you save your edits, giving `:r` will recompile the code on the fly. Calling `main` again will evaluate the changes.

#### Stochasticity

Our models are Bayesian by default, meaning that they allow for reasoning in probabilitic terms.

Practically, this is obtained by relying on the [Haskell Stochastic Package](https://hackage.haskell.org/package/stochastic), which employs monadic techniques.

A consequence of this is that deterministic strategic decisions (e.g. 'player chooses option A') must be lifted into the stochastic monad, getting thus transformed into their probabilistic equivalent (e.g. 'of all the options available, player chooses A with probability 1')

A practical example of this is the following:

```haskell
strategyName
  :: Kleisli
       Stochastic
       (Parameter1, Parameter2)
       Decision
strategyName = pureAction Decision1
```

In the example above, the player observes some parameters (`Parameter1` and `Parameter2` in this particular case), and then must assign an action (in this case `Decision1`).

`pureAction` lifts the deterministic choice `Decision1` to the corresponding concept in the probabilistic realm. 

The upside of assuming this little amount of overhead is that switching from pure to mixed strategies can be easily done on the fly, without having to change the model beforehand.

#### Branching

As a word of caution notice that, in a game with branching, we need to provide a possible strategy for each branch. For example, suppose to have the following game:

- Player 1 can choose between option A and B;
    - case A: Player 2 can choose between option A1 or A2;
    - case B: Player 2 can choose between option B1 or B2;

Moreover, suppose that the payoffs are as follows: 

- If Player1 chooses A, and then Player2 chooses A1, then both players get 100$.
- In any other case, both players get 0$.

In this game the best strategy is clearly (A,A1). Nevertheless, we need to supply a strategy for Player2 also in the 'B' branch: Even if Player1 will never rationally choose B, Player2 needs to be endowed with a clear choice between B1 and B2 in case this happens.


## File structure

The model is composed of several files, all on branch `master`. The file structure is as follows:

- The `app` folder contains `Main.hs`, where the `main` function is defined. This is the function executed when one gives `stack run` (cf. [Running the analytics](#running-the-analytics)).
- The `pics` folder exists only for the purpose of this documentation file.
- The `test` folders contain some basic Haskell testing code. Here 'test' has to be intended in the traditional development sense, that is, these are tests to check that the code works properly, and aren not about model analytics.

The code proper is contained in the `src` folder:
- `AuctionSupportFunctions.hs` defines all the auction procedures we are going to use as simple Haskell functions. Here we also define how payoffs are calculated.
- `Components.hs` defines subgames modelling reusable components such as bidders, which comprise the final games in `Model.hs`.
- `Model.hs` is where the fully assembled games are.
- `Strategies.hs` defines the strategies for all games in this folder. See [Supplying strategies](#supplying-strategies) for details.
- `Analytics.hs` contains the definition of equilibrium. These are automatically run by the `main` function when calling `stack run` in [Normal execution](#normal-execution) mode. Alternatively, one can call these functions directly while in [Interactive execution](#interactive-execution) mode, as in, for instance,
  ```haskell
    printEquilibriumDynamicAuction parametersJapaneseAuction 10 japaneseAuctionStrategyTuple initialAction
  ```
  Please refer to [Running the analytics](#running-the-analytics) for more information.
- `Diagnostics.hs` contains a set of helper functions that can be used to display more or less information about equilibrium, player moves and overall state of the game. These are useful to 'debug' a game in case of conceptually unexpected outputs.
- `Parametrization.hs` contains all the hardcoded exogenous parameters to be fed to the model, such as auction reserve price, players' names, initial players' endowments etc.
- `Types.hs` defines the types of many of the things we use in our model, such as private values, outcome of an auction, etc.

As an extra perk, we included the file `NestedAuctions.hs`, which contains some initial experiments around recursive auction systems.



# Analytics

Now, we switch focus on *analytics*, which we defined as the set of techniques we employ to verify if and when a supplied strategies results in an *equilibrium*. The notion of *equilibrium* we rely upon is the one of [Nash equilibrium](https://en.wikipedia.org/wiki/Nash_equilibrium), which intuitively describes a situation where, for each player, unilaterally deviating from the chosen strategy results in a loss.


## Reading the analytics

Analytics in our model are quite straightforward. In case a game is in equilibrium, the terminal will print `Strategies are in eqilibrium`.

For games with branching, there will also be a `NOTHING CASE`. To understand this, consider a game (call it `First Game`) that can trigger two different subgames (`Subgame branch 1`, `Subgame branch 2`, respectively) depending on the player's choice. Analytics would read like this:

```
 Game name
First Game:

 Strategies are in equilibrium
Subgame branch 1:

 NOTHING CASE
Subgame branch 2:

 Strategies are in equilibrium
```

Here `NOTHING CASE` signifies that the choice provided by the player results in not visiting `Subgame branch 1`, which is thus never played in this senario: Evidently, the choice made by the player in `First Game` resulting in the play continuing on `Subgame branch 2`.

On the contrary, analytics become more expressive when the game is *not* in equilibrium. In this case, the engine will suggest a more profitable deviation by displaying the following prompt:

```
Strategies are NOT in equilibrium. Consider the following profitable deviations: 

Player: 
Optimal Move: 
Current Strategy:
Optimal Payoff: 
Current Payoff: 
Observable State:
 --other game-- 
 --No more information--
```

`Observable State` contains a dump of all the game parameters that are currenlty observable by all players. This is usually a lot of information, mainly useful for debugging purposes. All the other field names are pretty much self-describing. 


## Strategies employed in the analysis

Here we report about the strategies we used in our tests. As one can see, since many of the prisoner's dilemma games we defined default to previously defined variations in some circumstances, we were able to make some strategic definitions only once and re-use them in more complicated models.

Essentially, each **Bidder** has two strategies. The first is to bid truthfully, meaning bidding exactly the value of the private valuation:
```haskell
-- | Truth telling strategy
truthTellingStrategy
  :: Kleisli
       Stochastic
       (Agent, PrivateValue)
       BidValue
truthTellingStrategy =
  Kleisli (\((_,value)) -> playDeterministically $ value)
```
The other one is to just bid a share of the private value. This corresponds to not being completely truthful about one's real valuation of the good on auction.
```haskell
-- | Bid a fraction of the valuation
-- NOTE this can be used for manually adjusting bids
bidShareOfValue
  :: Double
  -> Kleisli
       Stochastic
       (Agent, PrivateValue)
       BidValue
bidShareOfValue share =
  Kleisli (\((_,value)) -> playDeterministically $ roundTo 1 (share * value))
```

In the 'all pay' auction, we adopt the following strategy: If the private evaluation for **Bidder** is $0$ or $3$, the value is bid truthfully. If it is $6$ or $9$, a random value between $0$ and $3$ and $3$ and $6$, respectively, is played. This strategy may seem weird, but notice that underbetting makes sense in a context where also losing bidders have to pay, especially if the private valuation is high.

```haskell
bidAllPay
  :: Kleisli
       Stochastic
       (Agent, PrivateValue)
       BidValue
bidAllPay =
  Kleisli (\((_,value)) -> matchDiscreteSchedule  value)
  where matchDiscreteSchedule 0 = playDeterministically 0
        matchDiscreteSchedule 3 = playDeterministically 0
        matchDiscreteSchedule 6 = uniformDist [0,3]
        matchDiscreteSchedule 9 = uniformDist [3,6]
```


As for **Proposer** in the status quo model, the strategy consists in just picking the biggest bid:
```haskell
-- Proposer strategy
proposerMaxBid
  :: Kleisli
       Stochastic
       [Bid]
       Bid
proposerMaxBid = 
 Kleisli
   (\bidLS ->
      let maxBid = maximumBy (comparing snd) bidLS
          in playDeterministically $ maxBid)
```

For the Japanese auction, we leverage the fact that it is a [truthful mechanism](https://en.wikipedia.org/wiki/Strategyproofness): As such, each **Bidder** has a [dominant strategy](https://en.wikipedia.org/wiki/Japanese_auction#Strategies) consisting in leaving the auction only when the bid value exceeds the private valuation. We implemented this dominant strategy for each **Bidder**.

```haskell
-- | Truth telling strategy
-- Restrict the strategy space
participateBelowPriceStrategy
  :: Kleisli
       Stochastic
       (BidValue,Bool,Maybe PrivateNameValue)
       Bool
participateBelowPriceStrategy =
  Kleisli
    (\(value,bool,privateValueMaybe) ->
       case bool of
         False ->
           playDeterministically False
         True  ->
           case privateValueMaybe of
             Nothing -> playDeterministically False
             Just (_,privateValue) -> case value <=  privateValue of
                True  -> playDeterministically True
                False -> playDeterministically False)
```


## Running the analytics

As already stressed in [Evaluating strategies](#evaluating-strategies), there are two main ways to run strategies. In the [Normal execution](#normal-execution) mode, one just needs to give the command `stack run`. This command will execute a pre-defined battery of strategies using the parameters predefined in the source code. These parameters can be varied as one pleses. Once this is done and the edits are saved, `stack run` will automatically recompile the code and run the simulation with the new parameter set.

In the [Interactive execution](#interactive-execution) mode, the users accesses the repl via the command `stack ghci`. Here one can run single functions by just calling them with the relevant parameters, as in:

```haskell
functionName parameters
```

In particular, calling the function `main` in interactive mode will result in the same behavior of calling `stack run` in normal mode. Again, editing the source code and then hitting `:r` will trigger recompilation on the fly.

As we remarked early in this document, `main` runs both the simulation and equilibrium checking for each auction. One can use the functions `onlyEquilibria` and `onlySimulations` to just run the simulations or the equilibrium checking, respectively, for all auctions.


## Results

This section is an expansion of the section [analytics results](#analytics-results), which can be found at the top of this document.

As we remarked in [analytics results](#analytics-results), we implemented auctions that have been well-studied in the literature, and as such, we deemed that focusing on equilibrium would not be particularly insightful. For instance, we already know that in a Japanese auction and in a second price auction (more precisely in a [Vickrey auction](https://en.wikipedia.org/wiki/Vickrey_auction)), bidding truthfully results in equilibrium. As such, equilibrium checking of well-known auctions can be used to perform [sanity checks](#sanity-checks).

Having ascertained that focusing on equilibria would most likely amount to reinvent existing literature in auction theory, in this FRP the focus has been witched on [Simulations](#equilibrium-vs-simulations). The model allows to simulate auctions by specifying private valuations for all players and bidding strategies. The strategies can then be run, and the model determines the expected outcomes for bidders and auctioneer.

### Static auctions

As for the 'static auctions' (status quo, n-th price, all pay), the simulations are encapsulated in the following three functions:

- `printSimulationCurrentAuction`
- `printSimulationSimultaneousBidAuction`
- `printSimulationAllPayAuction`

The functions need to be fed some parameters and some strategies, as in

```haskell
printSimulationSimultaneousBidAuction parameters strategyTuple
```

Of the three, the first function runs the auction related to the current status quo, whereas the last runs the allpay auction. The second function simulates 1st, 2nd, nth price auction. The kind of auction we want to simulate is specified in the auction parameters. For instance, the following parameters, fed to `printSimulationSimultaneousBidAuction`, would define four identical players competing into a first price auction with zero reserve price (notice that the number of players is not changeable without also changing `Components.hs`):

```haskell
parametersFPAuction = Parameters
  { nameProposer =  "proposer"
  , name1 = "bidder1"
  , name2 = "bidder2"
  , name3 = "bidder3"
  , name4 = "bidder4"
  , valueSpace1 = privateValueLS
  , valueSpace2 = privateValueLS
  , valueSpace3 = privateValueLS
  , valueSpace4 = privateValueLS
  , actionSpace1 = actionLS
  , actionSpace2 = actionLS
  , actionSpace3 = actionLS
  , actionSpace4 = actionLS
  , reservePrice = zeroReservePrice
  , winningPrice = 1
  , approxError  = 0.4
  }
```

 Some predefined choices of parameters have been defined in `Parameters.hs` (see [File structure](#file-structure) for more information).

The output of these three functions is always a list of *expected bids*, computed as:

$$ \text{bid size} \cdot \text{probability the player has to pay} $$

Notice that in some auctions the players have to pay only when they win, and so the probability to pay equals the probability of winning the auction. In some other auctions, like the all pay auction, players have to pay also when they do not win.

Intuitively, expected bids mean the following: Suppose that a player has a private valuation (which in our case is a probability distribution, e.g. `privateValueLS` above). Each player has a strategy, which determines how much the player will bid given their private valuation. The game theoretic model  weights the amount paid with the probability of actually having to pay. So, for instance, if a player bids $10$, and if the player pays $50\%$ of the times, the expected bid will be $5$. Clearly, if the private valuation of each player is *certain*, then given any auction we can determine *with certainty* who the paying players will be. So, for instance, if we have two players with private valuation $10$ and $3$, respectively, in a first price auction, the expected bids will be $10$ and $0$, respectively.

Notice that, in our setting, players do not know other players' strategies, nor have *priors* on them. This means that the players themselves *do not know* their expected bids. The output of the functions above constitutes then a 'bird-eye' view that only the software engine has access to (and this is exactly why the software is useful!).

The expected payment to the auctioneer can be computed by just taking the sum of these expected bids. The idea here is the following: Imagine that in a game there are $n$ players, each bidding some amount, and each with a certain probability to pay. How much the auctioneer makes is computed as:

$$ \sum_{i=1}^n \text{bid player}_i \cdot \text{probability player}_i \text{ has to pay} $$

Which, as we said, is just the sum of the expected bids.

The following snippet of code prints expected bids and expected payment to auctioneer in a pretty way:

```haskell
putStrLn "List of expected bids: "
let expectedOutcome =  printSimulationFunction parameters strategyTuple
print expectedOutcome
putStrLn "Expected payment to auctioneer"
print $ sum expectedOutcome
```

With the outcome looking as:
```sh
List of expected bids: 
[bid1,bid2,bid3,...]
Expected payment to auctioneer
bid1 + bid2 + bid3 + ...
```

This is how `onlySimulations` in `Main.hs` is defined.

### Dynamic auctions

Dinamic auctions are more involved, as we have to take into account the fact that in a [Markov game](#markov-games) things repeat. The function `printSimulationRepeatedStageGame` prints simulations for a repeated game. In the case of a Japanese auction, the output would look something like this:

```haskell
[(Right ([("bidder1",False),("bidder2",False),("bidder3",True),("bidder4",True)],[("bidder1",False),("bidder2",False),("bidder3",True),("bidder4",True)],6.0,Just ("bidder1",5.0),Just ("bidder2",5.0),Just ("bidder3",7.0),Just ("bidder4",10.0),False,False,True,True),1.0)]
```

Here we have four players. `True` or `False` determine if a player is still part of the auction or not



### Sanity checks

As we briefly mentioned already, for auctions that have well-known equilibria, we used equilibrium checking as a form of sanity check. We verified equilibria for the current status quo, the first price, the second price, the all pay and the Japanese auction. The equilibria can be checked by running `onlyEquilibria` in [Interactive execution](#interactive-execution) mode.