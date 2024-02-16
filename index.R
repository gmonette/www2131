#' ---
#' title: "MATH 2131: Introduction to Statistics II"
#' date: "Winter 2024"
#' author: ""
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: 
#'       collapsed: false
#'     toc_depth: 3
#'     theme: readable
#' bibliography: 2131.bib
#' link-citations: yes
#' ---
#' \newcommand{\E}{\mathrm{E}}
#' \newcommand{\Var}{\mathrm{V\:\!\!ar}}
#' \newcommand{\mat}[1]{\begin{pmatrix}#1\end{pmatrix}}
#+ include=FALSE 
# TODO:
# 
# - First: possible ask list of questions for different students to answer
#   based on vocabular study.
#   Idea; Different students use same methodology so those lacking will
#   get help from others.
# 
# - look at notes in Quire for changes to description, e.g.
#   - require cameras for oral test
#   - in class 1 hour midterm and 2 hour final if inclass possible, otherwise
#     a 30 minute oral individually scheduled for both
#   - weekly quiz, in class on laptop with access to web to equalize
#     for students not in class
#   - sould I change to markdown to facilitate citations:
#   - add reading and sumarizzing a paper
#   - add contributed links should be accompanied with a note describing
#     why the link is interesting and providing a critique.
#   - front row team as before, your participation is much appreciated
#     and makes a big difference to the lecturer.
#   - Lescture will be on zomm. Class attendance is expected and participation
#     in class or via chat, etc, which students can use in class also is encouraged
#   - make sure speakers not active on your lapttop if you use zoom in class;
#     a good trick is to plug earbuds in your laptop to avoid deafening feedback
#     loops
#   - Team Project: differen students work on different aspects of a problem
#     and then combine their work into a common project and presentation to
#     be recorded on Zoom and will be posted for class.
#   - Two Other teams comment independently??
# What do do about SAS. Might have time this year due to early start.
# 
#' 
#+ include=FALSE
library(knitr)
k <- knitr::knit_exit
library(kableExtra)
library(magrittr)
# library(citr)
options(
  list(
    citr.bibliography_path='../2131.bib',
    citr.use_betterbiblatex = TRUE))
#' This version: `r format(Sys.time(), '%B %d %Y %H:%M')`
#' <FORM>
#' <INPUT TYPE="button" onClick="history.go(0)" VALUE="Click here to get the latest update">
#' </form>
#' <form method="post" action="#CURRENT">
#'    <button type="submit">Go to the current date</button>
#' </form>
#' 
#' <!--
#' 
#' <br>
#' <center>
#' <font color="#880000" size="5">___Announcement(s):___</font>
#' </center>
#' 
#' - No announcements so far
#' 
#' <font color="#880000" size="5">___The tutorial from 3 to 4 and office hours from 4 to 6 scheduled for Friday, January 22 
#' will take place at the same times on Saturday, January 23, instead ___</font>
#' <br><br>
#' <font color="#880000" size="5">___You must have passed<br>both MATH 3131 and MATH 4330<br>to enrol in and get credit for MATH 4939___</font>
#' 
#' > [Doubt is not a pleasant condition, but certainty is absurd. -- _Voltaire_](http://blackwell.math.yorku.ca/georgesmonette/files/quotes.html)
#' 
#' -->
#' 
#' > [Where there is no uncertainty, there cannot be truth -- _Richard Feynman_](http://blackwell.math.yorku.ca/georgesmonette/files/quotes.html)
#' 
#' > [To teach how to live without certainty, and yet without being paralysed by hesitation, is perhaps the chief thing that philosophy, in our age, can still do for those who study it. — Bertrand Russell](http://blackwell.math.yorku.ca/georgesmonette/files/quotes.html)
#' 
#+ include=FALSE
opts <- options(warn=-1)
library(p3d)
Init3d(cex=1)
d <- Smoking
d$`cigarettes/day` <- d$CigCon/365
d$`health spending` <- d$HealthExpPC
d$`life expectancy` <- d$LE
dim(d)
Plot3d(`life expectancy` ~ `health spending` + `cigarettes/day` | Continent, d)
spin(theta=-90,phi=0,fov=0)
Axes3d()
fit1 <- lm( `life expectancy` ~ `cigarettes/day`, d)
summary(fit1)
Fit3d(fit1, alpha = .5)

fith <- lm( `life expectancy` ~ `cigarettes/day` 
            + `health spending` 
            + log( `health spending`), d)

Fit3d(fith, col = 'red')
options(opts)
#+ echo=FALSE
rglwidget()
#' **How harmful is smoking?:** Cigarette consumption and life expectancy in 189 countries in 2004: Correlation is not causation. This is an example of the ecological fallacy, which is itself an example of a deeper phenomenon: Simpson's Paradox. 
#' 
#' <!-- > ![[<centre>Simpson's Paradox in 3D</centre>](http://blackwell.math.yorku.ca/Causality/rot.gif)](pix/simpsons.PNG) -->
#' 
#' # Quick links {#quick}
#' 
#' - [Go to current date](#CURRENT)
#' - [Course description](files/description.html)
#' - [Piazza class forum](http://piazza.com/yorku.ca/winter2024/math2131) 
#' - [Eclass -- only for grades](https://eclass.yorku.ca/)
#' - [Recordings](http://blackwell.math.yorku.ca/recordings/MATH2131/)
#' <!--- - [Questions](questions/m4939_questions_2024.html) --->
#' <!--- - [Annotated files on Mixed Models](files/Mixed_Models/)  --->
#'
#+ Calendar-------------
#'
#' # Calendar
#'
#' __Classes__ meet on Mondays, Wednesday and Friday from 11:30 to 12:20 in LSB 106 on Mondays and Wednesdays and in LSB 103 on Fridays.<br>
#' __Tutorial:__ On [Zoom](https://yorku.zoom.us/j/93888562389), on Fridays at 5:30 pm. Tutorials will be recorded.<br>
#' __Stats Help Center:__ See the schedule in [this post on Piazza](https://piazza.com/class/lpxid3wmydd7kh/post/208).<br>
#' __Instructor:__ [Georges Monette](http://blackwell.math.yorku.ca/gmonette)<br> 
#' __Teaching Assistants:__ My Luu, Zhenni Tan, Zixin Zhou<br>
#' __Email:__ Messages about course content should be posted publicly to Piazza. You may
#' post messages and questions as private messages to the instructors. If they are of
#' general interest, they will usually
#' be made public for the benefit of the entire class unless you specifically request that the message
#' remain private.
#' 
##- Day 1 ------
#'
#' ## __Day 1__: Monday, January 8
#' 
#' - [Course description](files/description.html)
#' - This evening, I will give access to Piazza to all students currently registered
#'   in the course. 
#'     -  I will use your York e-mail address. If you don't read 
#'        email at your York email address, please make sure that 
#'        it's forwarded to an email address that you do read regularly.
#'     -  Please do not change your e-mail address on Piazza because
#'        your York email address is used to identify you so you get credited for
#'        your contributions. 
#' - __Topic 1:__ How harmful is smoking?
#'
##- Day 2 ------
#'
#' ## __Day 2__: Wednesday, January 10
#' 
#' - Sorry! I forgot to record the last lecture. For those who
#'   couldn't make it, we discussed the [course description](files/description.html)
#'   and we decided to have a tutorial over [Zoom](https://yorku.zoom.us/j/93888562389) on Fridays at 5:30pm. 
#' - Do please remind me to record at the beginning of the lecture!
#' 
##- ### Assignment 1 (individual): Connecting with your team. ------
#' ### __Assignment 1__ (individual): Connecting with your team.
#' 
#' __Due Thursday, January 11, 9 pm__
#' 
#' - Join Piazza using the invitation sent to your yorku.ca email address.
#' - Get to know your team members. Post a message private to your team introducing yourself. What statistics
#'   courses have you taken? What programming languages do you know? Are you
#'   interested in particular applications of statistics, etc? Use the folder 'assn1'.
#' - Reply and comment on your team members' postings.
#' 
#' - Topic 1 --again: How harmful is smoking?
#'   - This is the [link to the R script used for the lecture.](files/Intro/Smoking.R)
#'
#'    
##- Day 3 ------
#'
#' ## __Day 3__: Friday, January 14
#' 
#' __Important:__ A few of you have not yet registered with Piazza and a few
#' who have registered have not yet sent an introductory message to their team.
#' You must send an introductory message for your team to know who you are.
#' Please __complete this today by 9pm__ at the very latest. Send me email at
#' georges@yorku.ca if you encounter technical obstacles.
#' 
#' __Chapters 1 and 2 of the textbook:__
#' 
#' - [Summary of Chapters 1 and 2 -- in progress](files/Chapters-1-2/Review.pdf)
#' - The first two chapters should be mainly a review of MATH 2030, occasionally
#'   going a bit deeper.
#' - The first few lectures will go through the first two chapters, highlighting
#'   key concepts and working on some exercises.
#' 
#' __Quiz on Wednesday, January 17:__
#' 
#' - In-class 10-minute paper quiz lasting 10 minutes, starting at 12:05 pm.
#' - Bring a blank sheet of paper, the question(s) will be shown on the screen.
#' - At the start of the quiz, the sheet should be blank except for your name 
#'   and student number.
#' - In fairness to your classmates, you __must stop writing__ when the end
#'   of the quiz is announced.
#' - If your grade on the final exam is higher than your quiz grade, for any
#'   quiz, then the grade on the final exam is substituted for your quiz
#'   grade.  Since the weight of missed quizzes is automatically transferred
#'   to your final exam, this rule is designed so you can't be penalized
#'   for choosing to write a quiz.
#' - I will discuss possible quiz questions during the lecture.   
#' 
#' __Formula sheet for the midterm and final exam:__
#' 
#' See this [Piazza post](https://piazza.com/class/lpxid3wmydd7kh/post/103) where
#' you can collectively create a formula sheet for the midterm and for the
#' final exam. As long as it can be printed on two pages, it will be included
#' in the midterm and in the final exam.  Of course, its contents might change
#' between the midterm and final.
#'  
##- ### Assignment 2 (team): ------
#' ### __Assignment 2__ (team): How harmful is smoking?
#' 
#' __Deadline:__ Wednesday, January 17, 8pm
#' 
#' Collectively with your team, discuss what principles are illustrated
#' by the Life Expectancy versus Smoking example. 
#' 
#' - What do they imply about the interpretation of statistical analyses?
#' - Some people would call the positive association between Smoking and Life Expectancy
#'   in the data we analyzed a _spurious association_. Yet it was a mathematically
#'   correctly computed association. Is the association itself really spurious? 
#'   What are people trying to say when they say it's spurious?
#'   How would you tell a spurious association from a non-spurious association?
#'   Is an association spurious just because you don't like it?
#' - When can an association between two variables be used as evidence
#'   that one variable causes the other? What's the difference between
#'   using a variable to 'predict', i.e 'guess', the value of another variable
#'   in contrast with concluding that intervening by changing one variable will result
#'   in a change of the other.       
#' - Prepare a statement on Piazza, private to your team, that expresses your team's opinion,
#'   by the deadline.
#' - Use the folder 'assn_2'.
#' - After the deadline, you may make your post public to the class. The instructors
#'   might comment and make some selected posts public.  
#'   
##- ### Assignment 3 (team): Chapters 1 and 2 ------
#' ### __Assignment 3__ (team): Connecting with your team.
#' 
#' - Deadlines (check the [course description](files/description.html) for the meaning of these deadlines)
#'     - Deadline #1: Monday, January 22 at 8pm
#'     - Deadline #2: Thursday, January 25 at 8pm
#'     - Deadline #3: Sunday, January 28 at noon
#' - Submitting work on Piazza:  You can upload, or cut and paste pictures of
#'   your work.  However, I encourage you to answer directly in
#'   Piazza and to use the LaTeX editor within Piazza for math. 
#'   See [this post in Piazza](https://piazza.com/class/lpxipx8lbrc6ho/post/30)
#'   about using LaTeX on Piazza.
#'   Add other resources to it if you find some.
#' - Assign each person in your group a number starting from 1 to the number
#'   of members in the group.  Use your alphabetical order based on your 
#'   first name.  If others join later, give them higher numbers, don't renumber
#'   the current members.
#' - Depending on your number, do the questions below by deadline 1 and
#'   post them on Piazza.
#'     - Use the folder 'assn_3'.
#'     - __Important:__ Use a __separate post__ for each question
#'       and use the title: 'Chapter X Problem Y' where X is the
#'       chapter number and Y is the problem number. Be sure to leave spaces
#'       exactly as shown.  Why should you do it this way? 
#'       Because when solutions are made public, you will be able to find
#'       all the solutions to the same problem together.
#'   
#'   | Number    | Chapter 1 Problems        | Chapter 2  Problems |
#'   |-----------|---------------------------|---------------------|
#'   |   1       |  7  33  62  70            |   5     16   33  62 |
#'   |   2       |  8  34  63  71            |  6     17    38 63  |
#'   |   3       |  9  35  64  74            |  11    24    39 64  |
#'   |   4       |  17 45  66  78            |   12    26    49 67 |
#'   |   5       |  20 49  68  79            |  13    29    60 68  |
#'   |   6       |  27 53  69  80            |  15    31    61 69  |
#'     
##- Day 4 ------
#'
#' ## __Day 4__: Monday, January 15
#' 
#' __Quiz on Wednesday, January 17:__
#' 
#' - In-class 10-minute paper quiz lasting 10 minutes, starting at 12:05 pm.
#' - __You may use a non-programmable calculator.__
#' - Bring a blank sheet of paper, the question(s) will be shown on the screen.
#' - At the start of the quiz, the sheet should be blank except for your name 
#'   and student number.
#' - In fairness to your classmates, you __must stop writing__ when the end
#'   of the quiz is announced.
#' - If your grade on the final exam is higher than your quiz grade, for any
#'   quiz, then the grade on the final exam is substituted for your quiz
#'   grade.  Since the weight of missed quizzes is automatically transferred
#'   to your final exam, this rule is designed so you can't be penalized
#'   for choosing to write a quiz.
#' - I will discuss the quiz question during the lecture.   
#' 
#' __Chapters 1 and 2 of the textbook (continuation):__
#' 
#' - [Summary of Chapters 1 and 2 -- Chapter 1](files/Chapters-1-2/Review_Chapter_1_annotated.pdf)
#' - [Summary of Chapters 1 and 2 -- Chapter 2 -- in progress](files/Chapters-1-2/Review_Chapter_2_annotated.pdf)
#' 
##- Day 5 ------
#'
#' ## __Day 5__: Wednesday, January 17
#' 
#' __Chapters 1 and 2 of the textbook (continuation):__
#' 
#' - [Summary of Chapters 1 and 2 -- Chapter 1](files/Chapters-1-2/Review_Chapter_1_annotated.pdf)
#' - [Summary of Chapters 1 and 2 -- Chapter 2 -- in progress](files/Chapters-1-2/Review_Chapter_2_annotated.pdf)
#' 
##- Day 6 ------
#'
#' ## __Day 6__: Friday, January 19
#' 
#' __Chapters 1 and 2 of the textbook (continuation):__
#' 
#' - [Summary of Chapters 1 and 2 -- Chapter 1](files/Chapters-1-2/Review_Chapter_1_annotated.pdf)
#' - [Summary of Chapters 1 and 2 -- Chapter 2 -- in progress](files/Chapters-1-2/Review_Chapter_2_annotated.pdf)
#' 
##- Day 7 ------
#'
#' ## __Day 7__: Monday, January 22
#' 
#' __Chapter 3 of the textbook (continuation):__
#' 
#' - [Chapter 3](files/Chapter-3/Chapter_3_annotated.pdf)
#' 
#' 
##- Day 8 ------
#'
#' ## __Day 8__: Wednesday, January 24
#' 
#' __Chapter 3 of the textbook (continuation):__
#' 
#' - [Chapter 3](files/Chapter-3/Chapter_3_annotated.pdf)
#' 
##- Day 9 ------
#'
#' ## __Day 9__: Friday, January 26
#' 
#' __Chapter 3 of the textbook (continuation):__
#' 
#' - [Chapter 3](files/Chapter-3/Chapter_3_annotated.pdf)
#' 
#' 
##- Day 10 ------
#'
#' ## __Day 10__: Monday, January 29
#' 
#' __Chapter 3 of the textbook (continuation):__
#' 
#' - The following link is to the folder for Chapter 3, in order to provide
#'   faster access to files.
#' - Original pdf files, without lecture annotations, include "original" in their names.
#' - Files with lecture annotations include "annotated" in their names.
#' - Ignore files with .xopp extensions unless you want to view them with 'xournalpp' app.
#'   
#' - [Chapters 3](files/Chapter-3/)
#' 
#' __Quiz on Wednesday, January 31:__
#' 
#' The quiz question will ask you to find a marginal density from a
#' joint density in which the support for $X$ depends on the value of $Y$ and vice-versa.
#' 
#' 
#' 
##- Day 11 ------
#'
#' ## __Day 11__: Wednesday, January 31
#' 
#' __Chapter 3 of the textbook (continuation):__
#'   
#' - [Chapters 3](files/Chapter-3/)
#' 
#' 
##- Day 12 ------
#'
#' ## __Day 12__: Friday, February 2
#' 
#' __Chapter 3 of the textbook (continuation):__
#'   
#' - [Chapters 3](files/Chapter-3/)
#' 
##- Day 13 ------
#'
#' ## __Day 13__: Monday, February 5
#' 
#' __Chapter 3 of the textbook (continuation):__
#' 
#' - [Chapters 3](files/Chapter-3/)
#' 
##- ### Assignment 4 (team): Chapter 3 ------
#' ### __Assignment 4__ (team):
#' 
#' - Deadlines (check the [course description](files/description.html) for the meaning of these deadlines)
#'     - Deadline #1: Monday, February 12 at 8pm
#'     - Deadline #2: Wednesday, February 14 at 8pm
#'     - Deadline #3: Friday, February 16 at 8pm
#' - Submitting work on Piazza:  You can upload, or cut and paste pictures of
#'   your work.  However, I encourage you to answer directly in
#'   Piazza and to use the LaTeX editor within Piazza for math. 
#'   See [this post in Piazza](https://piazza.com/class/lpxipx8lbrc6ho/post/30)
#'   about using LaTeX on Piazza.
#'   Add other resources to it if you find some.
#' - For this assignment, assign each person in your group a number starting from 6 downward towards 1
#'   using your alphabetical order based on your 
#'   first name.
#' - Depending on your number, do the questions below by deadline 1 and
#'   post them on Piazza.
#'     - Use the folder 'assn_4'.
#'     - __Important:__ Use a __single separate post__ for each question
#'       and use the title: 'Chapter X Problem Y' where X is the
#'       chapter number and Y is the problem number. Be sure to leave spaces
#'       exactly as shown.  Why should you do it this way? 
#'       Because when solutions are made public, you will be able to find
#'       all the solutions to the same problem together.
#'     - Use one and the same post for all your work on each question.
#'       Teams help each other using the follow-up discussions and the
#'       team member responsible for a question edits their own answer
#'       incorporating any suggestions or help they have received.
#'       You don't need to repost the your final answer. The answer is
#'       final because the date is past the last deadline.
#'   
#'   | Number    | Chapter 3 Problems     |
#'   |-----------|------------------------|
#'   |   1       |  1  12  27  44 66     |
#'   |   2       |  4  14  32  45 70      |
#'   |   3       |  5  15  34  48 72     |
#'   |   4       |  7 19  36  54  74    |
#'   |   5       |  8 21  38  56  77     |
#'   |   6       |  11 24 40 65 80     |
#'
#' 
##- Day 14 ------
#'
#' ## __Day 14__: Wednesday, February 7
#' 
#' __Chapter 3 of the textbook (continuation):__
#' 
#' - [Chapters 3](files/Chapter-3/)
#' 
##- Day 15 ------
#'
#' ## __Day 15__: Friday, February 9
#' 
#' __Chapter 4 of the textbook:__
#' 
#' - [Chapters 4](files/Chapter-4/)
#' 
##- Day 16 ------
#'
#' ## __Day 16__: Monday, February 12
#' 
#' __Chapter 4 of the textbook:__
#' 
#' - [Chapters 4](files/Chapter-4/)
#' 
##- Day 17 ------
#'
#' ## __Day 17__: Wednesday, February 14
#' 
#' __Chapter 4 of the textbook:__
#' 
#' - [Chapters 4](files/Chapter-4/)
#' 
##- Day 18 ------
#'
#' ## __Day 18__: Friday, February 16
#' 
#' __Chapter 4 of the textbook:__
#' 
#' - [Chapters 4](files/Chapter-4/)
#' 
##- ### Assignment 5 (team): Chapter 4 Part 1 ------
#' ### __Assignment 5__ (team) Chapter 4 Part 1:
#' 
#' - Not compulsory, recommended practice questions.
#' - Try them with your team.
#' - The team numbers are only for guidance if you choose to use them.
#' - Use the folder **assn_5** on Piazza.
#' 
#'   | Number    | Chapter 4 Part 1 Problems     |
#'   |-----------|------------------------|
#'   |   1       |  4  16  23      |
#'   |   2       |  5  17  25       |
#'   |   3       |  10  18  30       |
#'   |   4       |  12 20  31      |
#'   |   5       |  14 21  40       |
#'   |   6       |  15 22 43    |
#'


#' <!---
##- ### Assignment 5 (optional) ------
#' 
#' ### __Assignment 5__ (optional):
#' 
#' Some of you have requested exercises to practice R. These are
#' R assignments from a higher-level course but you are welcome to explore
#' them and discuss them on Piazza. There is no grade for trying this nor
#' a penalty for not doing so.
#' 
#' First, you should install R and RStudio for your operating system. 
#' 
#' It's challenging to find a good way to
#' 'learn R'.  It depend on where you are and where
#' you want to go. Now, there's a plethora of on-line
#' courses. See the blog post:
#' [The 5 Most Effective Ways to Learn R](https://www.r-bloggers.com/the-5-most-effective-ways-to-learn-r/) 
#' 
#' In my opinion, ultimately, the best way is to 
#' 
#' - __play__ your way through the 'official' 
#'   [manuals on CRAN](https://cran.r-project.org/manuals.html) 
#'   starting with 'An Introduction to R' along with
#'   'R Data Import/Export'. Note however that these materials
#'   were developed before the current mounting concern
#'   with reproducible research and some of the advice
#'   should be deprecated, e.g. using 'attach' and
#'   'detach' with data.frames.
#' - read the 
#'   [CRAN task views](https://cran.r-project.org/web/views) 
#'   in areas that interest you.
#' - Have a look at the over 500,000 questions tagged 'r' on
#'   [stackoverflow](https://stackoverflow.com/questions/tagged/r?tab=newest&page=1&pagesize=15). 
#' - At every opportunity, use R Markdown documents to work on assignments, project, 
#'   etc.
#'    
#' Using R is like playing the piano. You can read and learn
#' all the theory you want, ultimately you learn by __playing__.
#'
#' We will discuss 
#' the script 
#' [CAR_1.R](R/CAR_1/CAR_1.R). Copy it to a file in RStudio
#' and __play__ with it line by line. You can also run
#' the entire script with _Control-Shift-K_ to see how
#' scripts can be used for reproducible research.
#' 
#' #### Exercises:
#' 
#' - From [New 4939 questions](questions/m4939_questions_2022.html)
#'     - 5.1, 5.2, 5.3, 5.4, 5.5,
#'     - 5.6.23, 5.6.24, 5.6.25, 5.6.26, 5.6.27,
#'     - 6.1, 6.2, 6.3, 6.4, 6.5,
#'     - 7.4, 7.5, 7.6, 7.7, 7.8,
#'     - 8.1, 8.2, 8.3, 8.4, 8.5, 
#'     - 8.6, 8.7, 8.8, 8.9, 8.10,
#'     - 8.18.a, 8.18.b, 8.18.c, 8.18.d, 8.18.e, 
#'     - 8.36.a, 8.36.b, 8.36.c, 8.36.d, 8.36.e, 
#'     - 8.51.a, 8.51.b, 8.51.c, (write functions that would work on matrices of any size), 8.61.a, 8.62.a
#'     - 12.1, 12.3, 12.5, 12.7, 12.9, 
#'
#' ---> 
#' 
##+ CURRENT ----
#' <span id='CURRENT'></span>
#'    