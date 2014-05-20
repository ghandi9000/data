## These scripts assign z-values to each plant in the plots
Z-values are altitude values relative to the center of the plots.

### Methods to calculate z-values
1. $z = r\cos(\theta_B) \tan(\theta_S)$, where $\theta_B$ is the angle between the slope aspect and the vector from a given point through the origin, and $\theta_S$ is the slope angle.
  * $\theta_B$ is calculated from dot product of the projection of the slope aspect onto the 2d plot, $a$, and the vector from a given point through the origin, $b$:
  * $\theta_B = cos^{-1} ( \frac{ a \cdot b } {|a||b| } )$
  * Using this method, if $\theta_B > 90^{\circ}$, then $z$ will be a negative value, ie. downslope from the center of the plot.
  
### Moosilauke
On Moosilauke, each plot has an azimuth and slope along the azimuth.  Z-value are calculated from this information.

