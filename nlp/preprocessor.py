"""
Text Preprocessing Module
Handles tokenization, lemmatization, and text cleaning
"""

import spacy
from typing import List, Dict
import re


class TextPreprocessor:
    """
    Preprocesses text for emotion, intent, and context analysis
    
    Features:
    - Tokenization and lemmatization
    - Stop word removal (optional)
    - Punctuation handling
    - Lowercasing
    """
    
    def __init__(self, remove_stopwords: bool = False):
        """
        Initialize the preprocessor
        
        Args:
            remove_stopwords: Whether to remove stop words (default: False)
                             Keep False for emotion detection as words like "not", "very" matter
        """
        try:
            self.nlp = spacy.load("en_core_web_sm")
        except OSError:
            raise RuntimeError(
                "spaCy model 'en_core_web_sm' not found. "
                "Please run: python -m spacy download en_core_web_sm"
            )
        
        self.remove_stopwords = remove_stopwords
        
    def clean_text(self, text: str) -> str:
        """
        Basic text cleaning
        
        Args:
            text: Raw input text
            
        Returns:
            Cleaned text
        """
        # Remove extra whitespace
        text = re.sub(r'\s+', ' ', text)
        
        # Remove URLs
        text = re.sub(r'http\S+|www\S+', '', text)
        
        # Remove special characters but keep apostrophes for contractions
        text = re.sub(r'[^\w\s\']', ' ', text)
        
        return text.strip()
    
    def preprocess(self, text: str, lowercase: bool = True) -> str:
        """
        Full preprocessing pipeline
        
        Args:
            text: Raw input text
            lowercase: Whether to convert to lowercase
            
        Returns:
            Preprocessed text string
        """
        # Clean text
        text = self.clean_text(text)
        
        # Process with spaCy
        doc = self.nlp(text.lower() if lowercase else text)
        
        # Extract lemmas
        tokens = []
        for token in doc:
            # Skip stopwords if requested
            if self.remove_stopwords and token.is_stop:
                continue
            
            # Skip punctuation and spaces
            if token.is_punct or token.is_space:
                continue
                
            tokens.append(token.lemma_)
        
        return " ".join(tokens)
    
    def tokenize(self, text: str) -> List[str]:
        """
        Tokenize text into words
        
        Args:
            text: Input text
            
        Returns:
            List of tokens
        """
        doc = self.nlp(text.lower())
        return [token.text for token in doc if not token.is_punct and not token.is_space]
    
    def get_entities(self, text: str) -> List[Dict[str, str]]:
        """
        Extract named entities from text
        
        Args:
            text: Input text
            
        Returns:
            List of entities with type and text
        """
        doc = self.nlp(text)
        return [
            {"text": ent.text, "label": ent.label_}
            for ent in doc.ents
        ]
    
    def get_pos_tags(self, text: str) -> List[Dict[str, str]]:
        """
        Get part-of-speech tags
        
        Args:
            text: Input text
            
        Returns:
            List of tokens with POS tags
        """
        doc = self.nlp(text)
        return [
            {"token": token.text, "pos": token.pos_, "tag": token.tag_}
            for token in doc
            if not token.is_punct and not token.is_space
        ]


# Singleton instance for easy import
_preprocessor_instance = None

def get_preprocessor(remove_stopwords: bool = False) -> TextPreprocessor:
    """
    Get or create singleton preprocessor instance
    
    Args:
        remove_stopwords: Whether to remove stop words
        
    Returns:
        TextPreprocessor instance
    """
    global _preprocessor_instance
    if _preprocessor_instance is None:
        _preprocessor_instance = TextPreprocessor(remove_stopwords=remove_stopwords)
    return _preprocessor_instance
