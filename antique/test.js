$( function () {
    /* ========== Lazy Plugins ========== */
    
    LazyCollectionView = Backbone.View.extend( {
        
        bindTo : function( dispatcher, event, callback ) {
            this.bindings.push( { dispatcher : dispatcher, 
                                  event : event,
                                  callback : callback } );
            dispatcher.on( event, callback, this );
        },


        initialize : function( args, expand, self ) {
            
            this.collection = args.collection;

            this.bindings = new Array();

            _.bindAll( this, "lazyAdd" );
            _.bindAll( this, "lazyRemove" );
            _.bindAll( this, "lazyReset" );

            this.bindTo( this.collection, "add", this.lazyAdd );
            this.bindTo( this.collection, "remove", this.lazyRemove );
            this.bindTo( this.collection, "reset", this.lazyReset );

            this._viewList = new Array();

            this.collection.each( this.lazyAdd );
            
            if ( undefined != expand ) {
                expand.call( this, args );
            }
            
        },
        
        lazyAdd : function( model ) {
            var view = new this.collectionView( { model : model } );
            this._viewList[model.cid] = view;
            this.lazyRender( view );
        },

        lazyRemove : function( model ) {
            this._viewList[model.cid].terminate();
            delete this._viewList[model.cid];
        },

        lazyReset : function( e ) {
            for ( view in this._viewList ) {
                this._viewList[view].terminate();
                delete this._viewList[view];
            }
        },

        lazyRender : function( view ) {},
        
        terminate : function( event ) {
            this.remove();
            this.trigger( "terminate", event );
            this.undelegateEvents();
            for ( var view in this._viewList ) {
                this._viewList[view].terminate();
                delete this._viewList[view];
            }
        }
    } );

    LazyView = Backbone.View.extend( {
	
	initialize : function ( args, expand, self ) {

            this.bindings = new Array();
            
	    if ( undefined != expand ) {
                expand.call( this, args );
            }
	},

        bindTo : function( dispatcher, event, callback ) {
            this.bindings.push( { dispatcher : dispatcher, 
                                  event : event,
                                  callback : callback } );
            dispatcher.on( event, callback, this );
        },
        
        terminate : function( event ) {
            this.remove();
            this.trigger( "terminate", event );
            this.undelegateEvents();
            for ( item in this.bindings ) {
                this.bindings[item].dispatcher.off( null, null, this );
            }
        }

	
    } );



    /* ========== Definition Stage ========== */
    
    MyButton = LazyView.extend( {
        
	tagName: 'button',
        
        template: '<%= caption %>',
        
        events: {
            "click" : "onClick"
        },

        initialize: ( function( args, expand, self ) {
	    ( function ( cont ) {
		if ( undefined == self ) {
		    this.constructor.__super__.initialize.call( 
			this, 
			args, 
			cont,
			this.constructor.__super__ );
		} else {
		    self.constructor.__super__.initialize.call( 
			this, 
			args, 
			cont,
			self.constructor.__super__ );
		}
	    } ).call( this, function( args ) {
		this.caption = args.caption;
	    } );
		    
            if ( undefined != expand ) {
                expand.call( this, args );
            }
        } ),

        render: function() {
            this.$el.html( _.template( this.template, { caption: this.caption } ) );
            return this;
        },
        
        onClick: function() {}
        
    } );
    
    
    ButtonState = Backbone.Model.extend( {
        initialize : function ( args ) {
            this.caption = args.caption;
            this.msg = args.msg;
        }
    } );

    SignalButton = MyButton.extend( {

        initialize : function( args, expand, self ) {
	    ( function ( cont ) {
		if ( undefined == self ) {
		    this.constructor.__super__.initialize.call( 
			this, 
			args, 
			cont,
			this.constructor.__super__ );
		} else {
		    self.constructor.__super__.initialize.call( 
			this, 
			args, 
			cont,
			self.constructor.__super__ );
		}
	    } ).call( this, function( args ) {
		this.model = args.model;
		this.msg = args.model.msg;
		this.caption = args.model.caption;
	    } );
	    
	    if ( undefined != expand ) {
                expand.call( this, args );
            }
        },
        
        onClick: function() {
            this.model.trigger( "clicked", this.msg );
        }
    } );
    

    StateSet = Backbone.Collection.extend( {
        model: ButtonState
    } );
    
    
   
   


    ButtonPanel = LazyCollectionView.extend( {
        
        collectionView : SignalButton,

        lazyRender : function( view ) {
            $('#main').append( view.render().el );
        },

        initialize : function ( args, expand, self ) {
	    ( function ( cont ) {
		if ( undefined == self ) {
		    this.constructor.__super__.initialize.call( 
			this, 
			args, 
			cont,
			this.constructor.__super__ );
		} else {
		    self.constructor.__super__.initialize.call( 
			this, 
			args, 
			cont,
			self.constructor.__super__ );
		}
	    } ).call( this, function( args ) {

                this.bindTo( this.collection, "clicked", function( e ) {
		    this.collection.reset();
		    this.terminate( e );
		} );
	    } );

	    if ( undefined != expand ) {
                expand.call( this, args );
            }
        }
    } );

    /* ========== Compile Stage ========== */

      GenButton = MyButton.extend( {
	initialize : function( args, expand, self ) {
	     ( function ( cont ) {
		if ( undefined == self ) {
		    this.constructor.__super__.initialize.call( 
			this, 
			args, 
			cont,
			this.constructor.__super__ );
		} else {
		    self.constructor.__super__.initialize.call( 
			this, 
			args, 
			cont,
			self.constructor.__super__ );
		}
	    } ).call( this, function( args ) {
		$('#main').append( this.render().el );
	    } );
	    
	    if ( undefined != expand ) {
                expand.call( this, args );
            }
	},
        onClick: function() {

            var state_yes = new ButtonState( { msg : "yes", caption : "Yes" } );
            var state_no = new ButtonState( { msg : "no", caption : "No" } );
            
            var panel = new ButtonPanel( { collection : new StateSet( [state_yes, state_no] ) } );
            
            this.undelegateEvents();
            this.bindTo( panel, "terminate", function( e ) {
                // continuation starts here
                console.log( "the user clicked " + e );
                // continuation ends here
                this.delegateEvents();
            } );
        }
    } );
    
    

    var btn0 = new GenButton( { caption: "Generate" } );
    
    
} );

				       
