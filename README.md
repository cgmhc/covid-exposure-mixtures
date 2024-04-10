# mixture_models_covidmh-rf1-2d

This project aims to investigate the combined effects of multiple pandemic-related policy exposures on mental health outcomes using mixture modeling techniques. **The main objectives are:**

    To estimate the joint effects of COVID-19 policy exposures on depression and anxiety outcomes.
    To identify which components of the containment policies had the greatest impact on mental health outcomes.
    To compare and contrast different mixture modeling approaches, including quantile g-computation, targeted learning mixture analysis, and          
    cross-validated targeted maximum likelihood estimation.

Mixture modeling is particularly relevant to this project as it allows for the analysis of multiple exposures that are simultaneously intervened upon, better capturing real-world scenarios where correlated exposure variables generate combined health impacts. This approach will help uncover the cumulative effects of policy exposures on mental health outcomes, even when individual policies may have small effects when evaluated independently.

_Key research questions include:_

    What is the combined effect of multiple pandemic-related policy exposures on the incidence of depression and anxiety?
    Which specific policy exposures, or combinations of exposures, have the greatest impact on mental health outcomes?
    How do different mixture modeling techniques compare in terms of their ability to estimate the joint effects of policy exposures on mental health outcomes?


**Data**

The primary cohort for this analysis is the All of Us Research Program, with secondary cohorts including the MGB Biobank, UK Biobank, and ELSA Brasil. The data structure includes:

    Outcomes: Incident depression and anxiety diagnoses based on ICD-10 codes.
    Exposures: Policy exposures from the Oxford COVID-19 Government Response Tracker (OxCGRT), including school closings, workplace closings, travel restrictions, and others.
    Covariates: Individual-level covariates such as age, sex, race, ethnicity, socioeconomic status, and pre-existing mental health conditions, as well as location-level covariates.

The exposure window for this analysis is proposed to be 5 months (for the _minimum viable product_ analysis so that we can examine the effect of our exposure on a continuous outcome _(anxiety and depression severity, measured using GAD-7 and PHQ-9)_, with the outcome window following the exposure window. To handle the longitudinal nature of the policy exposures, we plan to:

    Binarize each policy exposure into "low" and "high" categories.
    Calculate a "time at high" variable that counts the number of days each policy was in the high category during the exposure window.
    Explore potential strategies for reducing the longitudinal nature of the exposures, such as binning, or taking the mean over the 
    exposure window.

By transforming the longitudinal exposures into continuous variables, we can then apply mixture modeling techniques to estimate the joint effects of these exposures on mental health outcomes.




## Add your files

- [ ] [Create](https://docs.gitlab.com/ee/user/project/repository/web_editor.html#create-a-file) or [upload](https://docs.gitlab.com/ee/user/project/repository/web_editor.html#upload-a-file) files
- [ ] [Add files using the command line](https://docs.gitlab.com/ee/gitlab-basics/add-file.html#add-a-file-using-the-command-line) or push an existing Git repository with the following command:

```
cd existing_repo
git remote add origin https://gitlab.partners.org/center-for-precision-psychiatry/mixture_models_covidmh-rf1-2d.git
git branch -M main
git push -uf origin main
```

## Integrate with your tools

- [ ] [Set up project integrations](https://gitlab.partners.org/center-for-precision-psychiatry/mixture_models_covidmh-rf1-2d/-/settings/integrations)

## Collaborate with your team

- [ ] [Invite team members and collaborators](https://docs.gitlab.com/ee/user/project/members/)
- [ ] [Create a new merge request](https://docs.gitlab.com/ee/user/project/merge_requests/creating_merge_requests.html)
- [ ] [Automatically close issues from merge requests](https://docs.gitlab.com/ee/user/project/issues/managing_issues.html#closing-issues-automatically)
- [ ] [Enable merge request approvals](https://docs.gitlab.com/ee/user/project/merge_requests/approvals/)
- [ ] [Set auto-merge](https://docs.gitlab.com/ee/user/project/merge_requests/merge_when_pipeline_succeeds.html)

## Test and Deploy

Use the built-in continuous integration in GitLab.

- [ ] [Get started with GitLab CI/CD](https://docs.gitlab.com/ee/ci/quick_start/index.html)
- [ ] [Analyze your code for known vulnerabilities with Static Application Security Testing (SAST)](https://docs.gitlab.com/ee/user/application_security/sast/)
- [ ] [Deploy to Kubernetes, Amazon EC2, or Amazon ECS using Auto Deploy](https://docs.gitlab.com/ee/topics/autodevops/requirements.html)
- [ ] [Use pull-based deployments for improved Kubernetes management](https://docs.gitlab.com/ee/user/clusters/agent/)
- [ ] [Set up protected environments](https://docs.gitlab.com/ee/ci/environments/protected_environments.html)

***


## Visuals
Depending on what you are making, it can be a good idea to include screenshots or even a video (you'll frequently see GIFs rather than actual videos). Tools like ttygif can help, but check out Asciinema for a more sophisticated method.

## Installation
Within a particular ecosystem, there may be a common way of installing things, such as using Yarn, NuGet, or Homebrew. However, consider the possibility that whoever is reading your README is a novice and would like more guidance. Listing specific steps helps remove ambiguity and gets people to using your project as quickly as possible. If it only runs in a specific context like a particular programming language version or operating system or has dependencies that have to be installed manually, also add a Requirements subsection.

## Usage
Use examples liberally, and show the expected output if you can. It's helpful to have inline the smallest example of usage that you can demonstrate, while providing links to more sophisticated examples if they are too long to reasonably include in the README.

## Support
Tell people where they can go to for help. It can be any combination of an issue tracker, a chat room, an email address, etc.

## Roadmap
If you have ideas for releases in the future, it is a good idea to list them in the README.

## Contributing
State if you are open to contributions and what your requirements are for accepting them.

For people who want to make changes to your project, it's helpful to have some documentation on how to get started. Perhaps there is a script that they should run or some environment variables that they need to set. Make these steps explicit. These instructions could also be useful to your future self.

You can also document commands to lint the code or run tests. These steps help to ensure high code quality and reduce the likelihood that the changes inadvertently break something. Having instructions for running tests is especially helpful if it requires external setup, such as starting a Selenium server for testing in a browser.

## Authors and acknowledgment
Show your appreciation to those who have contributed to the project.

## License
For open source projects, say how it is licensed.

## Project status
If you have run out of energy or time for your project, put a note at the top of the README saying that development has slowed down or stopped completely. Someone may choose to fork your project or volunteer to step in as a maintainer or owner, allowing your project to keep going. You can also make an explicit request for maintainers.
