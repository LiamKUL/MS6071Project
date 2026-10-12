# MS6071Project
Repository for project for MS6071 - R for Statistical Data Science.
Group members - Val Cluney, Liam Kelly, Kallen McCarthy

Our goal is to use Machine Learning methods to identify fractures and highlight their location in x-rays. This has strong applications in the diagnostic field and could help speed up diagnoses and make the job of medical professionals easier. Once we have the main code for finding fractures done, we will evaluate our models and try pick the best one. After that we plan to implement this code in a shiny app to make it usable in the medical field.

## Table of contents
- [Repository Architecture](#Repository-Architecture)
- [What we need for this](#What-we-need-for-this)
- [Quick Start Guide](#Quick-Start-Guide)
- [Contributing](#Contributing)



## Repository Architecture
```text
├── MS6071\
│   ├── code/          # All code related stuff
```
## What we need for this
R and RStudio obv
Github account etc which we already did
GitBash downloaded to computer. This is the important part. This is the command window for Github on your computer. Commands can be run here to get the stuff from the repository, and to push stuff you've done to the repository.

## Quick Start Guide
**Clone the repository:**
Set location in gitbash that you want the repository to be. Put in your own filepath. I have put mine in for example.
  ```bash
    cd "C:\Users\kelly\OneDrive - University of Limerick\Documents\College\Masters_Sem1\MS6071\MS6071 Code"
  ```
Then clone the repository to that location
   ```bash
     git clone https://github.com/LiamKUL/MS6071Project
   ```
This will import it to your pc storage. Then all changes can be made here and pushed to github. See next

## Contributing
Not going to be anything too strict about naming branches or labelling commits. As long as generally they say what it does, I think we should be g
**First time, only do on first setup**
Apparently we should all configure our names and emails. Idk really why but tbh I'm copying the FSUL one
   ```bash
     git config --global user.name "Your Name"
     git config --global user.email "your_student_id@studentmail.ul.ie"
   ```
Clone repository
Set location in gitbash that you want the repository to be. Put in your own filepath. I have put mine in for example.
  ```bash
    cd "C:\Users\kelly\OneDrive - University of Limerick\Documents\College\Masters_Sem1\MS6071\MS6071 Code\MS6071Project"
  ```
Then clone the repository to that location
   ```bash
     git clone https://github.com/LiamKUL/MS6071Project
   ```
This will import it to your pc storage. Then all changes can be made here and pushed to github. See next

**Everytime workflow. Do this stuff everytime you do something**
1. Open repository
   ```bash
    cd "Filepath of folder that IS repository"
   ```
2. Pull the latest changes merged by the rest of the team:
  ```bash
    git checkout main
    git pull origin main
  ```
3. Create and switch to a branch with somewhat descriptive name:
  ```bash
    git checkout -b whatever-you-want-to-call-branch
  ``` 
4. Make changes locally
   Work in that folder as you normally would
5. Save, commit, and push your changes
  In Git Bash, stage and commit your changes:
  ```bash
    # 1. Stage all new and modified files
    git add .

    # 2. Commit your changes with a name
    git commit -m "Something that kinda generally explains it"

    # 3. Push your branch to GitHub
    git push -u origin name-of-branch
  ```
This can be done everytime you work. The next bit is the kinda final bit that makes it part of the whole repository
6. Open a pull request
In browser, open a pull request. 
NOW, I'm not the most sure on this bit and how we should do it but I think we should make it so we can all open them and then close them straight away. Doing that will just keep it clear that changes have been made and I think it specifies exactly what was changed. We can then merge the branch and close it and open a new one for the next chnages. And sorry everything I've done so far has been done without pull requests yet cuz idk how they're setup and I want to see that before I use one.
We also could potentially set it up so at least one person has to review each merge but we can discuss that.
