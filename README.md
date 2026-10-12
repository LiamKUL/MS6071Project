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
