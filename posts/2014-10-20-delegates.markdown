---
title: Sending Data Back to a Previous View Controller
headerImg: 2014-10-20-delegates.png
---

##tl;dr
Sending data to a previous view controller sucks. Follow this recipe.

##Sending Data Forward (PrepareForSegue:)
Sending data to a new view controller is easy and can be done in as little as two lines using the prepare for segue method as shown below:

``` objective-c
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    SecondViewController *secondVC = segue.destinationViewController;
    secondVC.<propertyToSet> = <dataToBePassedForward>;
}
```

Passing data isn't just a one way street though. More often than not, we're going to want to return something in order to display it on the view controller we just returned to. For example, let's say we have a tableview populated with names. We want to be able to present a new view controller modally in order to type in a new name and add it to the list. In order to this this, we are going to need a way to pass that name from the modally presented view controller back to the presenting view controller. The best way to do this is by using a delegate. This tutorial will walk you through how to do that.

##Initial Setup
In order to follow along, I recommend you download the starter project I made from [here](https://github.com/ismailmustafa/PassingDataBack). Once you open this project, open up storyboard and you'll see two view controllers, one of which is embedded in a navigation controller. The root view controller is a table containing individual names and the second view controller is accessed via the **add** button on the top right of the **NamesTableViewController**. If you go ahead and run the app, you'll notice you can type a name into the text field but nothing happens when you click submit. We're going to set that up right now.

![Non-working Example](/static/img/image1-delegates.gif)

##How it Works
The whole point of this is to make the **NamesTableViewController** a delegate of **AddNameViewController**. We will then implement the delegate methods in **AddNameViewController**, and when the delegate method is used, **NamesTableViewController** will be notified through that particular delegate method. Re-written in a much more user friendly way where **Second View Controller** refers to the view controller we are going to:

1. Implement a delegate method in the **Second View Controller** using a protocol.
2. Create a delegate property that any other view controller can use to become a delegate of this **Second View Controller**.
3. Implement the delegate method in the **Second View Controller**
4. Have the **First View Controller** adopt the newly created protocol in its header file.
5. Implement the newly defined delegate method in the **First View Controller**.
6. Finally, set the **First View Controller** to be a delegate of the **Second View Controller**

##1. Create a Protocol
First we need to create a protocol in **AddNameViewController**. Add the following code right above **@interface** in the header file.
    
``` objective-c
@protocol AddNameViewControllerDelegate <NSObject>

- (void)addedNewName:(NSString *)name;

@end
```

##2. Create a Delegate Property

Create a delegate property so that we can use it to set the **NamesTableViewController** as the delegate.

``` objective-c
@interface AddNameViewController : UIViewController

@property (strong, nonatomic) id <AddNameViewControllerDelegate> delegate;

@end
```

##3. Implement Delegate Method in AddNamesViewController

Modify the submitButtonTapped: method in **AddNamesViewController.m** to look like this:

``` objective-c
- (IBAction)submitButtonTapped:(id)sender {
    [self.delegate addedNewName:self.nameTextField.text];
    [self dismissViewControllerAnimated:YES completion:nil];
}
```

The first method is called on self.delegate which means that if another view controller has implemented this method and is a delegate of this view controller, that method will "fire off". The second method simply dismisses the view controller so we can go back to the table view.

##4. Adopt AddNameViewControllerDelegate in NamesTableViewController

In **NamesTableViewController.h**, adopt the newly created protocol like so:

``` objective-c
#import "AddNameViewController.h"

@interface NamesTableViewController : UITableViewController <AddNameViewControllerDelegate>

@end
```

##5. Implement the Delegate Method in NamesTableViewController

If you go to **NamesTableViewController.m**, you'll notice a warning triangle at the top. This is because you have yet to implement the delegate method from the protocol you just adopted. So let's go ahead and do that just below **viewDidLoad**:

``` objective-c
- (void)addedNewName:(NSString *)name
{
    [self.names addObject:name];
    [self.tableView reloadData];
}
```

This method simply takes the **name** that was passed to it from **AddNameViewController** and adds it to the **self.names** NSMutableArray. We then reload the table view so that it updates whenever we add a name.

##6. Set the Delegate!!!
This is the most important step and can be easily overlooked. In the prepareForSegue: method found in NamesTableViewController.m, add the following:

``` objective-c
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    AddNameViewController *addNameVC = segue.destinationViewController;
    addNameVC.delegate = self;
}
```

First, we get an instance of the view controller we are "going to" by using the destinationViewController property on segue. Then we simply set the delegate property of AddNameViewController to self.

##Recap
Run the app, add a name, and when you click submit, you should see the name added to the list. While it seems like a lot, the amount of code added was a total of 12 lines. To recap, all we are doing here is enabling a first view controller to be a delegate of a second view controller so that if any delegate methods are used in the second view controller, the first view controller will know about it immediatley and "receive" whatever was passed into the delegate method.

![Working Example](/static/img/image2-delegates.gif)
