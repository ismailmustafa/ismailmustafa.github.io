---
title: How to Create Your Own CocoaPod
headerImg: 2014-10-27-cocoapods.png
---

The official CocoaPods website has a [great article](http://guides.cocoapods.org/making/making-a-cocoapod.html) on how to create a CocoaPod. While detailed, I had a lot of trouble following it because the information was very scattered and there wasn't much of a clear flow especially for a beginner like me. As a result, I've decided to attempt to make a guide that you can use to create your first CocoaPod.

##Creating the Pod Skeleton
Open up terminal and type the following command in:

``` bash
$ pod lib create <pod name>
```
    
You will be asked the following four questions:

``` bash
Would you like to have a demo for your library? [ Yes / No ]
```
    
The rule of thumb for this is if you need a screenshot in your readme file to show how it works, then you probably need a demo. If you have a demo, people will be able to **pod try** your library. Keep in mind that **pod try** still works even you you choose No in this step, there just won't be anything to demo when the user clicks **run**.

``` bash
Which testing frameworks will you use? [ Specta / Kiwi / None ]
```
    
This will install pods for the selected testing framework.

``` bash
Would you like to do view based testing? [ Yes / No ]
```

This is for more complicated visual pods where you need to ensure that a certain action leads to a correct view being displayed. The official CocoaPods website recommends using [FBSnapShotTestCase](https://github.com/facebook/ios-snapshot-test-case). This is a testing framework made by Facebook that compares a screenshot of the contents on the screen after an action is performed to a reference image that is stored in the source code's mainbundle. If the two images don't match, then the test fails. This is a really interesting way of easily testing really complicated view controllers that contain a lot of views. You can learn more about FBSnapShotTestCase and snapshot testing in general [here](http://www.objc.io/issue-15/snapshot-testing.html).

The final question you are asked is:

``` bash
What is your class prefix?
```
    
This is just a really convenient way of CocoaPods prefixing all files in the project with your chosen class prefix.

As an example I selected Specta as my testing framework and opted in for view based testing. This is what was downloaded:

![Installed dependencies for pod lib](/static/img/image1-cocoapods.png)

As expected, the expecta frameworks were automatically installed and since I opted in for view based testing (and CocoaPods recommends facebook's snapshot framework), FBSnapshotTestCase was installed. After the library is created, the <your-pod's-name>.xcworkspace file will open automatically.

##Understanding the Generated Template

![Standard CocoaPod template](/static/img/image2-cocoapods.png)

From top to bottom: 

***Podspec Metadata***

This folder contains the .podspec which basically describes everything about your pod. It also has the README.md and an auto-generated license. If you look at the **s.license** property in the .podspec file, you'll notice that the default license is 'MIT'. You can learn more about licences [here](http://choosealicense.com/).

***samplePod***

This is where your sample project goes.

***Tests***

All your test files go here. The default skeleton code's syntax is based on what testing framework you picked. Since I picked Specta, mine looks like this:

![Specta testing framework](/static/img/image3-cocoapods.png)

***Development Pods***

This is where you put the files you created for your library.

***Pods***

All the pods that were installed during the creation of this template go here. Also, if the pod you are trying to create requires other pods to run, they'll be in here as well.

##Creating a Pod Homepage

Creating a pod homepage is as simple as creating a github public repository that has (typically) the same name as your pod, then setting it as the origin of the library you just created. So go ahead and create a github public repo. You don't need to create the repository with a readme or a license since that's already created for you. 

Now back in your template, **git init** and **git commit** to create an initial commit. Then add the remote you just created on github. This is what I would do:

``` bash
git init
git add -A
git commit -m "Initial commit"
git remote add origin <link-to-repository>
git push origin master
```
    
##Adding Your Files

your untouched directory should look like this in finder:

![Untouched directory](/static/img/image4-cocoapods.png)

Place any files relating to your library in the **Classes** folder. Then add the files to your project in this location:

![Location to add files](/static/img/image5-cocoapods.png)

You can now create a sample project using your library.

##Editing the .podspec File

The .podspec should be very straight forward to edit. A sample of how I edited mine is show below:

``` bash
Pod::Spec.new do |s|
  s.name             = "samplePod"
  s.version          = "0.1.0"
  s.summary          = "A pod to teach people how to make a CocoaPod"
  s.description      = <<-DESC
                       This pod does absolutley nothing.
                       DESC
  s.homepage         = "https://github.com/ismailmustafa/samplePod"
  s.license          = 'MIT'
  s.author           = { "ismailmustafa" => "ismailmustafa@rocketmail.com" }
  s.source           = { :git => "https://github.com/ismailmustafa/samplePod.git", :tag => s.version.to_s }

  s.platform     = :ios, '7.0'
  s.requires_arc = true

  s.source_files = 'Pod/Classes/*.{h,m}'
  s.resources = 'Pod/Classes/*.{txt}'
  s.resource_bundles = {
    'samplePod' => ['Pod/Assets/*.png']
  }
end
```
    
You basically fill in the blanks but there are a few things I want to go into detail about. 

***s.version***

I didn't know anything about version control until I made my first CocoaPod, but it's very straight forward. CocoaPods refers to [this website](http://semver.org/) for information on how to version. CocoaPods uses the Semantic versioning specification for software versioning. Semantic versioning uses the pattern (MAJOR.MINOR.PATCH).

- MAJOR: This is when your changes are incompatible with the previous version.
- MINOR: Adding a relatively big feature that is still backwards compatible.
- PATCH: Small bug fixes

***s.source_files***

This specifies all the files you need for your library. I added **/*.{h,m}** in order to explicitly specify to only add header and implementation files.

***s.resources***

This is where you specify resources that need to be copied in the build phase. This ensures that those files are added to **Copy Pods Resources** under **Build Phases**. Again, I added **/*.{txt}** just to specify the type of files I'm adding. 

##Editing the README File

Your README file should look like this:

![Readme file](/static/img/image6-cocoapods.png)

The readme file is almost fully prefilled for you. You should fill in usage details and any requirements that your pod needs. This is also where you should add any screen shots if you are to use any. 

##Submitting the Pod

After ensuring your tests have worked and you're ready to upload your pod, you need to tag your git repository with the proper version number as shown:

``` bash
git tag '0.1.0'
git push --tags
```

This will tag your latest commit with the specified version number and push that tag up to your github repo. Also, make sure that the **s.version** property in your .podspec file matches this version number. You'll also need to make sure you do this everytime you push updates. You'll increment the version number to whatever you want it to be in your .podspec, and then git tag your repo. You can learn more about git tagging [here](http://git-scm.com/book/en/v2/Git-Basics-Tagging).

In order to actually upload your pod, you'll need to register with CocoaPod trunk which is CocoaPod's authentication and API service. You can find out more about the CocoaPod trunk [here](http://guides.cocoapods.org/making/getting-setup-with-trunk). To register, type the following in your pod directory in terminal:

``` bash
$ pod trunk register <your-email> '<your name>' --description='macbook pro'
```

You will then receive an email where you'll need to click a verification link. After you click that link, you can simply type:

``` bash
$ pod trunk push
```
    
And congrats!! You've officially created your first CocoaPod. There's obviously a lot more detail to this and I've only just skimmed the surface. You can learn more about CocoaPods on their [official website's guide section](http://guides.cocoapods.org/).

##IJMRandomName

This is a CocoaPod that I made in order to teach myself how to create CocoaPods. Its only purpose is to generate a random full name. You can check it out [here](https://github.com/ismailmustafa/IJMRandomName).
