---
title: Creating a Hamburger Menu With the "Just in Mind" Wireframing Tool
---

I recently came across this really cool wireframing tool called [Just In Mind](http://www.justinmind.com/) which is awesome because you can create interactive demos of your app that look great. Some samples of what these live demos look like can be found [here](http://www.justinmind.com/examples).

##Don't Use Their Tutorial

You can find a hamburger menu tutorial created by Just In Mind [here](http://www.justinmind.com/support/slide-menu/), but I don't like the approach they took. If you notice, all they did was create the view, then when the button is pressed, it goes to an entirely new screen with the hamburger menu taking up a significant portion of the screen. While the effect is the same, during the middle of the animation, you see the right part of the first view repeating. This can easily be seen with their live demo [here](https://www.justinmind.com/usernote/tests/4/1765/12529460/index.html#/screens/68503a7d-e959-40d2-95b6-e671729aeb29). Just press the hamburger menu button several times until you notice the middle repeating part.

##Laying Out the Initial View Controller

My tutorial is a little more involved, but the result is so much more satisfying. Start by opening up Just In Mind and selecting iPhone 5 in portrait. Now lay out the main screen according to however you want it to look. I'm making an app for my sister's clothing boutique back in the U.A.E ([The Luxury Arcade](http://instagram.com/theluxuryarcade)), so I started by laying out a simple navigation bar at the top. You can find this in the widgets menu on the left side. Everything I added including the hamburger menu button and labels should be there. The navigation menu should be under "bars", and the hamburger button is under "icons". Labels can be found under "basic".

![Adding the navigation bar](/static/img/image1-hamburgerMenu.png)

Then I included three images of some twitter posts from the boutique's feed to make it look like a table view. Adding images is as simple as dragging it into the prototyping window.

![Adding twitter posts](/static/img/image2-hamburgerMenu.png)

Everything snaps to a grid so it's really easy to align views.

##Adding the Hamburger Menu

Attaching the hamburger menu is as easy as dragging a view to the left or right side so that it is off the screen. I like to put it on the left since that seems to be the more commonly used side. Start by adding an extension to the navigation bar but reducing the width to be more inline with how wide a hamburger menu should be. I picked an arbitrary width of 250 pixels since that's what I thought looked best.

![Add hamburger menu navigation bar](/static/img/image3-hamburgerMenu.png)

I also added a "menu" label just so it didn't look so bare. Now, add the rest of the hamburger menu items including a large rectangle to represent the body of the hamburger menu. After playing around for a while, I got mine to look like this:

![Final view before animation](/static/img/image4-hamburgerMenu.png)

Don't worry, the part that is not in the screen will not show up in the simulation. You can even check for yourself by clicking the green "simulate" button in the top right.

##Creating a Dynamic Panel

Since we can only animate single views, we are going to have to group everything into something called a dynamic panel. If you are familiar with photoshop, this is just like "flattening" an image.

![Dynamic panel](/static/img/image5-hamburgerMenu.png)

##Adding Another Hamburger Menu Button

Now that we have created a dynamic panel out of all the views we put together, we can no longer create an action associated with the initial button we placed. To fix this, we need to add another button directly on top of the initial one. Make sure it's perfectly in line with the previous button. You can use the arrow keys to move the position of the button as well. You may be thinking "why did we even group the button into the dynamic panel if we needed it to be active?". The answer is that we can only move a single view at a time, so if we didn't include the button in the dynamic panel, we would have had to move the hamburger menu into the screen and then place the button back in the right place which would result in a small period of time inbetween where the button dissapears on the screen. By including the button in the dynamic panel, we can ensure that during the animation, the button will appear to move along with the animation. This will make more sense later.

![Adding another menu bar button](/static/img/image6-hamburgerMenu.png)

Remember! You can align any view more precisely with the arrow keys.

##Adding Events

In order to create the animation, we need to create five events. The events will perform the following actions:

1. Hide the menu bar button item.
2. Move the hidden menu bar button item to its final correct position 250 pixels to the right.
3. Move the entire dynamic panel 250 pixels to the right.
4. Unhide the menu bar button item.

###1. Hide the Menu Bar Item

Click on the hamburger menu button. You'll notice at the bottom there is a menu with "event" as one of the tabs. Under the "Events" tab, click "Add Event". 

![Click add event button](/static/img/image7-hamburgerMenu.png)

Check to see that at the very top left, "On Tap" is selected since we want the action to be performed when we tap the button. On the left menu, click on "Show/Hide" and make sure the bar button item is still selected in the "Outline" pane on the right. Then select the "Hide" radio button at the top and click okay.

![Add event menu](/static/img/image8-hamburgerMenu.png)

You'll notice that a new event has been added with "Hide" written underneath it. You've just officially hidden the bar button item. If you click simulate, you won't be able to see it happening since the other bar button item is still behind it but I assure you that it is indeed hiding. 

![New event added](/static/img/image9-hamburgerMenu.png)

###2. Move the hidden menu bar button item to its final correct position 250 pixels to the right

Right click on the new action you just created and click "Add Action". This will add another action that will occur right after the "hide" action. You can add as many actions as you like and they will all be performed in the order that you added them.

![Adding a new action](/static/img/image10-hamburgerMenu.png)

This time, select "Move" on the left side since we want to move the button. Make sure the bar button item is still selected in the outline. Change the radio button to "To Position". Then adjust the left value to 258. This effectively moves the bar button 250 pixels to the right. Finally, set the "Easing" to "None". This ensure that the buttons is not animated to its final position and that no time is wasted moving it to its final position. We are doing this because button is hidden and there is no need to animate something that is hidden.

![Modifying the action](/static/img/image11-hamburgerMenu.png)

###3. Move the Entire Dynamic Panel 250 Pixels To the Right

Add yet another action but this time with the following parameters:

![Dynamic panel movement settings](/static/img/image12-hamburgerMenu.png)

Select move in the left panel. Check the "By Offset" radio button and set the offset to 250. The easing should be set to "Linear" and finally, leave the duration at its default value of 500 ms. Make sure that the dynamic panel is selected in the outline on the right this time.

###4. Unhide the Menu Bar Button Item

Add another action with the following settings:

![Show button action](/static/img/image13-hamburgerMenu.png)

All we are doing here is unhiding the bar button item which is the opposite of the first action. Awesome! So now when we test it in the simulator, clicking the button makes the hamburger menu appear. Alright what happens if we click the button again?

![Not yet finished](/static/img/image14-hamburgerMenu.gif)

Uh oh... We never created an event to reverse everything, so it just repeats the event we previously created and keeps moving to the right another 250 pixels.

##Create Another Screen

Before we can reverse the action, we need to create a new screen so that we have a button with no events attached to work with. In the top right, click the plus button under the tab "Screens":

![Adding a new screen](/static/img/image15-hamburgerMenu.png)

Name the screen "Menu Shown", or whatever else you want to call it as long as it makes sense to you:

![Menu Shown](/static/img/image16-hamburgerMenu.png)

Now copy everything from the "Main Screen" including the extra bar button you added. Just highlight everything. Now paste it into the new "Menu Shown" screen and position so it looks like the menu is being shown:

![Menu shown screen](/static/img/image17-hamburgerMenu.png)

##Link the Main Screen to the Menu Shown Screen

Go back to the "Main Screen" and click on the hamburger menu button. Now add a final new action under the same event. This time, select the first option in the left pane "Link To" and then select the "Menu Shown" screen under "Screens". Leave everything else the same or as shown in the picture and press okay.

![Link main screen to menu shown screen](/static/img/image18-hamburgerMenu.png)

Now go into the new "Menu Shown" screen, click on the hamburger bar button, and delete all the actions associated with it. You can do this by clicking on the red 'X' under the events tab:

![delete actions](/static/img/image19-hamburgerMenu.png)

This will make sure that the button doesn't cause the view to animate to the right another 250 pixels.

![New screen](/static/img/image20-hamburgerMenu.gif)

##Reversing the Process

Now we are going to reverse the series of actions we just created except we will do this on the new screen we made ("Menu Shown"). The steps we will take are as follows:

1. Hide the menu bar button item.
2. Move the hidden menu bar button item to its final correct position 250 pixels to the left.
3. Move the entire dynamic panel 250 pixels to the left.
4. Unhide the menu bar button item.
5. Link the "Menu Shown" screen back to the "Main Screen"

###1. Hide the Menu Bar Button Item
Click on the hamburger bar button and add a new event. Create an action with the same settings as you did before:

![hide menu button](/static/img/image21-hamburgerMenu.png)

###2. Move the Hidden Menu Bar Button Item to Its Final Correct Position 250 Pixels to the Left

Once again this is the same as before except we are now setting the value of "Left" to 8 pixel, the original distance the hamburger button was from the left:

![Move menu button back](/static/img/image22-hamburgerMenu.png)

###3. Move the Entire Dynamic Panel 250 Pixels to the Left

Once again, all the settings are the same except this time, the offset value is -250 (notice the negative sign). This is because we are moving it to the left.

![Move dynamic panel back](/static/img/image23-hamburgerMenu.png)

###4. Unhide the Menu Bar Button Item

Repeat step one but select "show" this time.

![Unhide the menu bar Button](/static/img/image24-hamburgerMenu.png)

###5. Link the "Menu Shown" Screen Back to the "Main Screen"

Finally, we want to create a link back to the "Main Screen" so that when we click the button again, it performs the correct action as dictated by the main screen. This way the button should cause the hamburger menu to toggle open and closed.

![link menu shown to main menu](/static/img/image25-hamburgerMenu.png)

##And Finally We're Done!

The hamburger menu opens and closes as it should without that extra repeating panel in the middle of the animation.

![Final app](/static/img/image26-hamburgerMenu.gif)
