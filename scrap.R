
elems_topnavbar3 <- elemsByClassNameCompound(client,'D_BC M_UK')
elems_topnavbar3_child1 <- elems_topnavbar3[1]

elems_topnavbar3_child1


elems_topnavbar3_child1_child <- elems_topnavbar3_child1$html_children()

#dropdown
option <- remDr$findElement(using = 'xpath', "//select[@aria-label='Language select']/option[@value='en']")




modal <- client$findElement(using="id",id_of_modal)
seleniumPipes::findElementsFromElement(modal,using="xpath", value="//input[@aria-label='Cat 1")





nav_all_cat <- elemsByAriaLabel(client,V_NAVBAR_TEXT_ALL_CATEGORY)
nav_all_cat2 <- elemsByAriaLabel(client,V_NAVBAR_TEXT_ALL_CATEGORY_CH)
fff <- elemsByAriaLabel(client,'Carousel')


elemsByClassNameCompound <- function(parent,n) {
  
  arg1 <-  paste("//input[@class = '", n, "']", sep="")
  print(c("[elemsByClassNameCompound] ", arg1))
  
  
  elems <- parent$findElements('xpath',arg1)
  return(elems)
}

elemsFind <- function(parent, uses, n) {
  return(parent$findElements(using=uses, n))
}
elemsByClassName <- function(parent,n) {
  
  arg1 <-  n
  print(c("[elemsByClassName] ", arg1))
  
  
  elems <- elemsFind(parent,'class name',arg1)
  return(elems)
}

elemsByAriaLabel <- function(parent,n) {
  arg1 <-  paste("//input[@aria-label='", n, "']", sep="", collapse = "")
  
  print(c("[elemsByAriaLabel] ", arg1))
  elems <- elemsFind(parent,'xpath',arg1)
  return(elems)
}



elems_topnavbar3 <- elemsByClassName(client,'D_BC')

V_NAVBAR_TEXT_FASHION = 'Fashion'
V_NAVBAR_TEXT_ALL_CATEGORY = 'All Categories'
V_NAVBAR_TEXT_ALL_CATEGORY_CH = '所有分類'




ssss <- client$findElements(using="xpath",'//input[@aria-label="Carousel"]')


# 注册

#base::lapply(elems_topnavbar, function(elem){
#  print(elem)
#})

#aria-label
#所有分類


#text
#client$findElement(using = 'xpath', '//*[text()="所有分類"]')



getSubCategoryList <- function(c_) {
  
  getScript <- function(category) {
    return (
      paste("
    const class_name_category_list_items = \"D_I_ D_hS\";
    
    const categories = document.getElementsByClassName(class_name_category_list_items);
    for (const item of categories) { 
    
    	const descNode = item.children[1];
    	const descText = descNode.innerText;
    	if (descText.localeCompare(\"", category, "\") == 0) {
    	
    		const elementParent = item.parentElement;
    		const elementSiblings = elementParent.children;
    		for(const meOrSibling of elementSiblings) {

    			if ( meOrSibling.classList.contains(\"D_IA\")  ){
    				const button = meOrSibling;
    				
    				button.click();
        		const elementsParentLevel = elementParent.parentElement.children;
    		    
    		    
    		    const l_ = []
      		  for(const e_ of elementsParentLevel) {
      		  
    			      if (!(e_.classList.contains(\"D_IB\") && e_.classList.contains(\"D_hS\"))){
    			        
                  // NOT A SUBCATEGORY
    			      }else {
    			      
    			      return e_;
    			        l_.push(e_);
    			      }
      		  } 
      		  
      		  
      		  return l_;

      		  

    			}
    		}
    		

    		
    		
    		return null;
    	}
    }
    ", sep="")
    )
    
  }
  parent_list <- client$executeScript(getScript(c_))
  print(parent_list)
  parent_list
  return(parent_list)
}

for (sub_cat in sub_cats){
  print("[extraction] sleeping")
  Sys.sleep(5)
  print("[extraction] click")
  sub_cat$clickElement()
  
}
