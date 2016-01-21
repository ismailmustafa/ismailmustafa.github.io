---
title: Introduction to Core Motion
---

Recently, I've been playing with the coremotion framework provided by apple in order to get accelerometer and gyroscope updates from my phone. Setting it up is actually surprisingly easy. 

##1. Import CoreMotion

``` objective-c
#import <CoreMotion/CoreMotion.h>
```

##2. Create CMMotionManager Property and Initialize it

First create a property:

``` objective-c
@property (strong, nonatomic) CMMotionManager *motionManager;
```

Then initialize it in viewDidLoad:

``` objective-c
self.motionManager = [[CMMotionManager alloc] init];
```

##3. Set Update Intervals

Both accelerometer updates and gyroscope updates use completion handlers to give you updates at whatever interval you pick so we need to setup an interval at which you want to receive updates. This is as simple as accessing the **accelerometerUpdateInterval** or **gyroUpdateInterval** for each:

``` objective-c
self.motionManager.accelerometerUpdateInterval = 0.2;
self.motionManager.gyroUpdateInterval = 0.2;
```

##4. Start Updates

Finally, add the two update methods afterwards to start updating. In the following setup, I've done it so that my accelerometer and gyro data are NSLogged once every 0.2 seconds.

``` objective-c
[self.motionManager startAccelerometerUpdatesToQueue:[NSOperationQueue currentQueue]
    withHandler:^(CMAccelerometerData *accelerometerData, NSError *error) {
    [self outputAccelertionData:accelerometerData.acceleration];
    if(error){
        NSLog(@"%f, %f, %f", acceleration.x, acceleration.y, acceleration.z);
    }
}];

[self.motionManager startGyroUpdatesToQueue:[NSOperationQueue currentQueue]
    withHandler:^(CMGyroData *gyroData, NSError *error) {
        NSLog(@"%f, %f, %f", gyroData.rotationRate.x, 
              gyroData.rotationRate.y, gyroData.rotationRate.z);
}];
```

