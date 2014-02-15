How to Use the sistanlp Code Repository with Github
===================================================

1. Read a github tutorial. There are many good ones on the web. The trickiest thing with Git is understanding branches. This tutorial might be helpful: http://git-scm.com/book/en/Git-Branching-Branching-Workflows. This is a more detailed tutorial, if you really want to understand how things work (although the instructions below might be sufficient for most things): http://ftp.newartisans.com/pub/git.from.bottom.up.pdf. 

2. To get a copy of sistanlp and start working with git, please follow these steps:

	`git clone git@github.com:sistanlp/sistanlp.git`

3. By default you will be in the "master" branch. To see what local branches you have, and which one you are in, type (the branch marked with "*" is the branch you are in):

	`git branch`

4. Please work from your own branch for every major change (see next chapter for best-practice rules). To create a new local branch, type:

	`git checkout -b my_new_branch`

5. After you make some changes, you can the status of files with `git status`. To add a new file and then commit it, use the instructions below. Note that, if you your comments are very verbose (and they should be!), it is best to use `git commit -a`: this will open a separate editor where you can type your commit message.

	`git add my_new_file`

	`git commit -am "description of my first commit"`

6. You have to publish the changes in your branch to a _remote_ branch, so that other people can see your contributions. You do this by:

	`git push origin my_new_branch`

7. When your changes are ready to be merged into the "master" branch, do:

	`git co master` (which switches to your local master branch)

	`git pull origin master` (which updates the local master with changes published in the remote master)

	`git merge my_new_branch` (merge your local branch into local master. **Resolve any conflicts reported by git here, if necessary.**)

	`git commit -a` (commit the merged master locally. **Summarize the changes made in your branch in the commit comments.** Note: this step is not necessary if the merge above completed successfully without any manual editing.)

	`git push origin master` (push the updated local master to remote master)

General Best-practice Rules for Github
======================================

* It is important to keep the remote master ("origin master") clean. That means, do not merge your branch back into master if it's not in a stable state.

* It is important to create a separate branch for every major change (i.e., research idea) you make. Merge branches back into master as soon possible, but only when: (a) the idea was proven to work, and (b) see previous rule.

* Do not create too many levels of branching, that is, branches spin-off from another branch. Keep your branch structure as flat as possible, ideally a one-level tree where every branch is a child of "master".

* Use your remote branch (step 6 in the previous chapter) as a place to backup your changes before merging back into master.

Useful Tips
===========

* To save some typing, edit your ~/.gitconfig files and add aliases, e.g.:

<blockquote>
[user]<br>
	&nbsp;&nbsp;email = msurdeanu@email.arizona.edu<br>
[alias]<br>
	&nbsp;&nbsp;st = status<br>
	&nbsp;&nbsp;ci = commit<br>
	&nbsp;&nbsp;br = branch<br>
	&nbsp;&nbsp;co = checkout<br>
	&nbsp;&nbsp;ls = ls-files<br>
	&nbsp;&nbsp;discard = checkout -- .<br>
[push]<br>
	&nbsp;&nbsp;default = tracking<br>
</blockquote>

* It is useful to display the current in your Bash command line (for *nix and Mac folk). You can add the following block to your .bashrc file:

<blockquote>
# first, copy scripts/git-completion.bash to ~/.git-completion.bash <br>
source ~/.git-completion.bash<br>
if [ "$USER" = "root" ]; then<br>
  &nbsp;&nbsp;PS1='\e[1;31m[\u@\h \W$(__git_ps1 " (%s)")]\$\e[0m '<br>
else<br>
  &nbsp;&nbsp;PS1='[\u@\h \w$(__git_ps1 " (%s)")]\$ '<br>
fi
</blockquote>

* If you messed up your current branch, did not commit the changes, and just want to revert to the last checkin, use:

	`git checkout -- .` or (if you added the alias above)

	`git discard`





