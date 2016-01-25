---
title: Hakyll on Github Pages using Stack
headerImg: 2016-01-24-hakyllSetup.png
---

If you've seen the dates on my previous posts, you'll know that I've been
delaying writing a post for quite a while. So, I decided to start the
new year with a blog refresh using Hakyll. I've also made it my
new years resolution to start writing more blog posts about the work I've
been doing.

In this post, I'm going to talk about how I set up Hakyll on Github 
Pages using Stack. For this tutorial, I'm assuming you already have 
Stack setup. If not, then get it 
<a href="http://www.haskellstack.org" target="_blank">here</a>.
While you don't actually need Stack to setup Hakyll, you'll need it for
this tutorial since the commands I'll be showing use Stack.

## Getting Started ##

First, create a repository on Github for your Github Pages site with the following name:

``` bash
<your-name>.github.io
```

Be sure to create it without a README, and don't clone it yet as we'll push our 
hakyll repo up to it later.

Before I move on, I'll explain exactly what we'll be doing. First we'll create a Hakyll
project using the Stack Hakyll template. Then we'll git init our directory and set it up
so that we have two branches, a master branch and a hakyll branch where all our work will
be done. The way github pages works is that it expects the index.html file to be in the top
directory however when we build the site using hakyll, it generate the entire site in the 
\_site/ subdirectory. Github pages won't pick this up. So in this setup, the master branch 
will contain the generated website, and the Hakyll branch will contain all the code required 
to build it. Then we'll create a script that will deploy the website for us without us having 
to switch back and forth between branches manually.

Back to the steps. We want to setup Hakyll using the Stack template for Hakyll:

``` bash
stack new <your-blog-name> hakyll-template
```

This will create a folder with the name <your-blog-name>. cd into this folder.
You'll notice that this template automatically generates a .gitignore file for 
you. If you're on a mac, you'll want to remember to add .DS\_Store to your 
.gitignore (this should ideally be in your global gitignore). 

## Setting your blog up with Git ##

The next few commands will set up your Git repo so that there is a clear separation
between your website on master, and the tools to generate your website on the
hakyll branch. Git init the repo, and add your remote to your github pages repo:

``` bash
git init
git add -A
git commit -m "Initial commit"
git remote add origin git@github.com:<github-name>/<github-name>.github.io.git
```

Then create a hakyll branch, and push it up to your blog:

``` bash
git checkout -b hakyll
git push -u origin hakyll
```

Now go back to your master branch, delete everything except for .git/ and .gitignore, 
and push that branch up as well.

``` bash
git checkout master
rm -rf *
rm .ghci
git add -A
git commit -m "Delete all files"
git push -u origin master
```

We've now setup our repo correctly and all we have to do is go back to the hakyll
(git checkout hakyll) branch and create our deploy script:

``` bash
touch deploy
chmod 700 deploy
```

And now open that file and copy the following into it:

``` bash
1  #!/bin/bash
2  shopt -s extglob
3
4  function error_exit
5  {
6      echo "$1" 1>&2
7      exit 1
8  }
9
10  # Check if commit message argument
11  if [ -z "$1" ]
12  then
13    echo "deploy script error: What changes did you make? Type a commit message."
14    exit 1
15  fi
16  
17  # Build successfuly or exit
18  stack clean
19  stack build || error\_exit "deploy script error: Build failed"
20  
21  # Push changes on hakyll branch to github
22  git add -A
23  git commit -m "$1" || error\_exit "deploy script error: no changes to commit"
24  git push origin hakyll
25  
26  # Switch to master branch
27  git checkout master
28  
29  # delete old site
30  rm -rf !(CNAME) # dont delete CNAME
31  git add -A && git commit -m "delete old site"
32  
33  # switch to hakyll branch and rebuild website
34  git checkout hakyll
35  stack exec blog rebuild
36  
37  # switch to master, extract site and push
38  git checkout master
39  mv \_site/\* .
40  git add -A && git commit --amend -m "$1"
41  git push origin master
42  
43  # return to original branch
44  git checkout hakyll
```

I won't go into too much detail because I feel as though I've commented it fairly well
but I'll give a quick summary. Lines 10 - 15 check if you've included a 
commit message as an argument to the script. It then checks if your project builds, 
in lines 17 - 19. Those changes are then commited and pushed to your hakyll branch.
After switching back to the master branch, we delete the old website but ensure
that the CNAME file isn't deleted as we need that for custom domains. We then commit this
with a generic message and switch back to the hakyll branch. We then invoke the hakyll
rebuild command to build your website which creates the \_site folder. Switching back to
master brings this folder along. We then extract the contents of this folder, commit the
changes, and push the new site up to origin.

## Workflow ##

So now that we have our git repo set up with the proper branches, and a convenient deploy script
that does all the work for us, what does this mean for our workflow? Well I now never have to leave
the hakyll branch, and whenever I make any changes like writing a new blog post, all I have to
do is type the following command:

``` bash
./deploy "Wrote new blog post"
```
