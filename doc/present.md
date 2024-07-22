---
title: Haskell on Web
author: Mehran
---

Haskell On Web
---

<!-- column_layout: [1, 2, 1] -->

<!-- column: 1 -->
<!-- new_lines: 10 -->
# Phase 1
<!-- pause -->
## Backend
* REST
* Postgresql
* Model Designing
* Authentication (jwt)
* Memory Caching
* Session management
<!-- pause -->
## Front
* Login page
* Game form
* Finding avatar https://iconscout.com/all-assets/mafia
* Role description
* Role distribution
* Game status

# Phase 2
* Multi user access to game using websocket

<!-- end_slide -->
What we manage to do
---
<!-- column_layout: [1, 2, 1] -->

<!-- column: 1 -->
<!-- new_lines: 10 -->
## Set up a dev workflow (used Nix)
<!-- pause -->
## Project Management using Kanban board
<!-- pause -->
## Setup Handler and define routes
<!-- pause -->
## Set up a simple Schema and run on Sqlite
<!-- pause -->
## Connect React to our web
<!-- pause -->
## Using the tempalte to create some pages

<!-- end_slide -->
Setup Handler
---
```haskell

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")
```
<!-- pause -->
Setup rest route using Model
---
```haskell
postCommentR :: Handler Value
postCommentR = do
    comment <- (requireCheckJsonBody :: Handler Comment)

    maybeCurrentUserId <- maybeAuthId
    let comment' = comment { commentUserId = maybeCurrentUserId }

    insertedComment <- runDB $ insertEntity comment'
    returnJson insertedComment

getCommentR :: Handler Value
getCommentR = do
  -- Fetch comments from the database
    comments <- runDB $ selectList [] [Desc CommentId]
  -- Convert comments to JSON
    let commentsJson = Aeson.toJSON comments
  -- Return comments as JSON response
    returnJson commentsJson
```
<!-- end_slide -->
Purity
---
```typescript
import { RoleCard } from '../routes/rolecards';

const RoleCardComponent = ({ data }: { data: RoleCard }) => {
    return (
        <div>
            {data ? (
                <div key={data.name}>
                    <img className="w-full" src={data.avatar}
                     alt={data.name} />
                    <div className="px-6 py-4">
                        <p>{data.name}</p>
                        <p>{data.player}</p>
                        <p>
                          {data.mafia ? 'Mafia' : 'Not Mafia'}
                        </p>
                    </div>
                </div>
            ) : (
                <p>No more data to display.</p>
            )}
        </div>
    );
};

export default RoleCardComponent;

```
<!-- end_slide -->
Handling Side Effects in Event Handlers
---
```typescript

  const onSubmit = useCallback(async (data: FormData) => {
    try {
      // Side effect: Making an API call
      const response = await axios.post(apiUrl + '/login', {
        email: data.email,
        password: data.password,
      });

      // Side effect: Logging the response
      console.log('Login successful:', response.data);

      // Side effect: Storing token in local storage
      localStorage.setItem('token', response.data.bearerToken);

      // Side effect: Navigating to another page
      navigate('/');
    } catch (error) {
      console.error('Error:', error);
      if (axios.isAxiosError(error) && error.response) {
        setError(error.response.data.message || 
        'An error occurred during log-in');
      } else {
        setError('An unknown error occurred');
      }
    }
  }, [navigate, apiUrl]);

```
<!-- end_slide -->
Passing Props to a Component
---
```typescript
const Voting: React.FC = () => {
   

   ...

    return (
        <div>
            <div className="flex flex-wrap justify-center">
                <VotingComponent 
                player={players[currentIndex]} 
                players={players} 
                onSelectionChange={handleSelectionChange} />
            </div>
            
            ...

        </div>

    );
};

export default Voting;
```

<!-- end_slide -->
Reading Props from component
---
```typescript

type VotingComponentProps = {
    player: Player;
    players: Player[];
    onSelectionChange: (isChekced: boolean, 
    playerId:number, selectedId: number) => void;
  };

const VotingComponent = ({ player, players,
 onSelectionChange }: VotingComponentProps) => {
    const [checkedItems, setCheckedItems] = useState<string[]>([]);

    const handleCheckboxChange = 
    (event: React.ChangeEvent<HTMLInputElement>,
     playerId:number, selectedId: number) => {
        const isChecked = event.target.checked;
        const item = `${playerId}-${selectedId}`;

        if(isChecked) {
            setCheckedItems([...checkedItems, item]);
        }
        else {
            console.log(checkedItems);
            setCheckedItems(checkedItems
            .filter(checkedItem => checkedItem !== item));
        }

        onSelectionChange(isChecked, playerId, selectedId); 
        
      };

    return (
      ...
    );
};

export default VotingComponent;
```
